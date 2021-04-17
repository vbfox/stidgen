﻿module BlackFox.MasterOfFoo.Build.Tasks

open Fake.Api
open Fake.BuildServer
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Tools

open BlackFox
open BlackFox.Fake
open System.Xml.Linq
open Fake.DotNet.Testing

let createAndGetDefault () =
    let configuration = DotNet.BuildConfiguration.fromEnvironVarOrDefault "configuration" DotNet.BuildConfiguration.Release

    let projectName = "BlackFox.Stidgen"
    let testProjectName = projectName + ".Tests"
    let rootDir = System.IO.Path.GetFullPath(__SOURCE_DIRECTORY__ </> ".." </> "..")
    let srcDir = rootDir </> "src"
    let artifactsDir = rootDir </> "artifacts"
    let nupkgDir = artifactsDir </> projectName </> (string configuration)
    let projectFile = srcDir </> projectName </> (projectName + ".fsproj")
    let projectBinDir = artifactsDir </> projectName </> (string configuration)
    let solutionFile = srcDir </> "stidgen.sln"
    let projects =
        GlobbingPattern.createFrom srcDir
        ++ "**/*.*proj"
        -- "*.Build/*"

    /// GitHub info
    let gitOwner = "vbfox"
    let gitHome = "https://github.com/" + gitOwner
    let gitName = "stidgen"

    let getUnionCaseName (x:'a) =
        match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with | case, _ -> case.Name

    let release =
        let fromFile = ReleaseNotes.load (rootDir </> "Release Notes.md")
        if BuildServer.buildServer <> BuildServer.LocalBuild then
            let buildServerName = (getUnionCaseName BuildServer.buildServer).ToLowerInvariant()
            let nugetVer = sprintf "%s-%s.%s" fromFile.NugetVersion buildServerName BuildServer.buildVersion
            ReleaseNotes.ReleaseNotes.New(fromFile.AssemblyVersion, nugetVer, fromFile.Date, fromFile.Notes)
        else
            fromFile

    Trace.setBuildNumber release.NugetVersion

    let writeVersionProps() =
        let doc =
            XDocument(
                XElement(XName.Get("Project"),
                    XElement(XName.Get("PropertyGroup"),
                        XElement(XName.Get "Version", release.NugetVersion),
                        XElement(XName.Get "PackageReleaseNotes", String.toLines release.Notes))))
        let path = artifactsDir </> "Version.props"
        System.IO.File.WriteAllText(path, doc.ToString())

    let init = BuildTask.create "Init" [] {
        Directory.create artifactsDir
    }

    let clean = BuildTask.create "Clean" [init] {
        let objDirs = projects |> Seq.map(fun p -> System.IO.Path.GetDirectoryName(p) </> "obj") |> List.ofSeq
        Shell.cleanDirs (artifactsDir :: objDirs)
    }

    let generateVersionInfo = BuildTask.create "GenerateVersionInfo" [init; clean.IfNeeded] {
        writeVersionProps ()
        AssemblyInfoFile.createFSharp (artifactsDir </> "Version.fs") [AssemblyInfo.Version release.AssemblyVersion]
    }

    let build = BuildTask.create "Build" [generateVersionInfo; clean.IfNeeded] {
        DotNet.build
          (fun p -> { p with Configuration = configuration })
          solutionFile
    }

    let runTests = BuildTask.create "Test" [build] {
        let testsBinaryDir = artifactsDir </> testProjectName </> (string configuration) </> "net5.0"
        [ testsBinaryDir </> (testProjectName + ".dll") ]
            |> Expecto.run (fun p ->
                { p with
                    PrintVersion = false
                    FailOnFocusedTests = true
                })
        Trace.publish (ImportData.Nunit NunitDataVersion.Nunit) (testsBinaryDir </> "TestResults.xml")
    }

    let nuget = BuildTask.create "NuGet" [build] {
        DotNet.pack
            (fun p -> { p with Configuration = configuration })
            projectFile
        let nupkgFile =
            nupkgDir
                </> (sprintf "stidgen.%s.nupkg" release.NugetVersion)

        Trace.publish ImportData.BuildArtifact nupkgFile
    }

    let publishNuget = BuildTask.create "PublishNuget" [nuget] {
        let key =
            match Environment.environVarOrNone "nuget-key" with
            | Some(key) -> key
            | None -> UserInput.getUserPassword "NuGet key: "

        Paket.push <| fun p ->  { p with WorkingDir = nupkgDir; ApiKey = key }
    }

    let zipFile = artifactsDir </> (sprintf "%s-%s.zip" projectName release.NugetVersion)

    let zip = BuildTask.create "Zip" [build] {
        let comment = sprintf "%s v%s" projectName release.NugetVersion
        GlobbingPattern.createFrom projectBinDir
            ++ "**/*.dll"
            ++ "**/*.xml"
            -- "**/FSharp.Core.*"
            |> Zip.createZip projectBinDir zipFile comment 9 false

        Trace.publish ImportData.BuildArtifact zipFile
    }

    let gitHubRelease = BuildTask.create "GitHubRelease" [zip] {
        let user =
            match Environment.environVarOrNone "github-user" with
            | Some s -> s
            | _ -> UserInput.getUserInput "GitHub Username: "
        let pw =
            match Environment.environVarOrNone "github-pw" with
            | Some s -> s
            | _ -> UserInput.getUserPassword "GitHub Password or Token: "

        // release on github
        GitHub.createClient user pw
        |> GitHub.draftNewRelease
            gitOwner
            gitName
            release.NugetVersion
            (release.SemVer.PreRelease <> None)
            (release.Notes)
        |> GitHub.uploadFile zipFile
        |> GitHub.publishDraft
        |> Async.RunSynchronously
    }

    let gitRelease = BuildTask.create "GitRelease" [] {
        let remote =
            Git.CommandHelper.getGitResult "" "remote -v"
            |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
            |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
            |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

        Git.Branches.tag "" release.NugetVersion
        Git.Branches.pushTag "" remote release.NugetVersion
    }

    let _releaseTask = BuildTask.createEmpty "Release" [clean; gitRelease; gitHubRelease; publishNuget]
    let _ciTask = BuildTask.createEmpty "CI" [clean; runTests; zip; nuget]

    BuildTask.createEmpty "Default" [runTests]

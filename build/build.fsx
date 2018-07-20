﻿#r @"../packages/FAKE/tools/FakeLib.dll"
#load "../paket-files/vbfox/FoxSharp/src/BlackFox.FakeUtils/TypedTaskDefinitionHelper.fs"
#load "./AppveyorEx.fsx"

open Fake
open Fake.Testing.Expecto
open Fake.Core
open Fake.IO
open Fake.Tools
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.DotNet
open Fake.BuildServer
open System
open System.IO
open BlackFox
open BlackFox.TypedTaskDefinitionHelper

BuildServer.install [
    AppVeyor.Installer
]

#load "../packages/SourceLink.Fake/tools/Fake.fsx"

let configuration = "Release"
let rootDir = Path.GetFullPath(__SOURCE_DIRECTORY__ </> "..")
let artifactsDir = rootDir </> "artifacts"
let appBinDir = artifactsDir </> "bin" </> "BlackFox.Stidgen" </> configuration

let project = "stidgen"
let summary = "Strongly Typed ID type Generator"
let solutionFile  = rootDir </> project + ".sln"
let testAssemblies = artifactsDir </> "bin" </> "*.Tests" </> configuration </> "*.Tests.exe"
let sourceProjects = rootDir </> "src/**/*.??proj"

/// The profile where the project is posted
let gitOwner = "vbfox"
let gitHome = "https://github.com/" + gitOwner

/// The name of the project on GitHub
let gitName = "stidgen"

/// The url for the raw files hosted
let gitRaw = Environment.environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

// --------------------------------------------------------------------------------------
// Build steps
// --------------------------------------------------------------------------------------

let inline versionPartOrZero x = if x < 0 then 0 else x

// Read additional information from the release notes document
let release =
    let fromFile = ReleaseNotes.load (rootDir </> "Release Notes.md")
    if BuildServer.buildServer = BuildServer.AppVeyor then
        let appVeyorBuildVersion = int AppVeyor.Environment.BuildVersion
        let nugetVer = sprintf "%s-appveyor%04i" fromFile.NugetVersion appVeyorBuildVersion
        let asmVer = System.Version.Parse(fromFile.AssemblyVersion)
        let asmVer =
            System.Version(
                versionPartOrZero asmVer.Major,
                versionPartOrZero asmVer.Minor,
                versionPartOrZero asmVer.Build,
                versionPartOrZero appVeyorBuildVersion)
        ReleaseNotes.ReleaseNotes.New(asmVer.ToString(), nugetVer, fromFile.Date, fromFile.Notes)
    else
        fromFile

AppVeyorEx.updateBuild (fun info -> { info with Version = Some release.AssemblyVersion })

// --------------------------------------------------------------------------------------
// Clean build results

let clean = task "Clean" [] {
    Shell.cleanDir artifactsDir

    !! solutionFile
    |> DotNet.MSBuild.runRelease id "" "Clean"
    |> ignore
}

// Generate assembly info files with the right version & up-to-date information
let assemblyInfo = task "AssemblyInfo" [ clean.IfNeeded ] {
    let getAssemblyInfoAttributes projectName =
        [
            Fake.DotNet.AssemblyInfo.Title projectName
            Fake.DotNet.AssemblyInfo.Product project
            Fake.DotNet.AssemblyInfo.Description summary
            Fake.DotNet.AssemblyInfo.Version release.AssemblyVersion
            Fake.DotNet.AssemblyInfo.FileVersion release.AssemblyVersion
        ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! sourceProjects
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
        )
}

// --------------------------------------------------------------------------------------
// Build library & test project

let build = task "Build" [ assemblyInfo ] {
    !! solutionFile
    |> DotNet.MSBuild.runRelease id "" "Rebuild"
    |> ignore
}

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

let runTests = task "RunTests" [ build ] {
    !! testAssemblies
        |> Expecto (fun p ->
            { p with
                FailOnFocusedTests = true
                Parallel = false
            })
}

// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries http://ctaggart.github.io/SourceLink/

open SourceLink

let sourceLink = task "SourceLink" [ build ] {
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName
    Trace.tracefn "SourceLink base URL: %s" baseUrl

    !! sourceProjects
    |> Seq.iter (fun projFile ->
        let projectName = Path.GetFileNameWithoutExtension projFile
        let proj = VsProj.LoadRelease projFile
        Trace.tracefn "Generating SourceLink for %s on pdb: %s" projectName proj.OutputFilePdb
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb rootDir baseUrl
    )
}

let finalBinaries = EmptyTask "FinalBinaries" [ (if Environment.isMono then build else sourceLink) ]

// --------------------------------------------------------------------------------------
// Build a Zip package

let zipPath = artifactsDir </> (sprintf "%s-%s.zip" project release.NugetVersion)

let zip = task "Zip" [ finalBinaries ] {
    let comment = sprintf "%s v%s" project release.NugetVersion
    let files =
        !! (appBinDir </> "*.dll")
        ++ (appBinDir </> "*.config")
        ++ (appBinDir </> "*.exe")
    Fake.IO.Zip.createZip appBinDir zipPath comment 9 false files

    AppVeyor.PushArtifact (fun p ->
        { p with
            Path = zipPath
            FileName = Path.GetFileName(zipPath)
            DeploymentName = "Binaries"
        })
}

// --------------------------------------------------------------------------------------
// Build a NuGet package

let nuget = task "NuGet" [ finalBinaries ] {
    Fake.DotNet.Paket.pack <| fun p ->
        { p with
            OutputPath = artifactsDir
            Version = release.NugetVersion
            ReleaseNotes = String.toLines release.Notes
            WorkingDir = appBinDir }

    !! (artifactsDir </> "*.nupkg")
    |> Seq.iter (Trace.publish ImportData.BuildArtifact)
}

let publishNuget = task "PublishNuget" [ nuget ] {
    let key =
        match Environment.environVarOrNone "nuget-key" with
        | Some(key) -> key
        | None -> UserInput.getUserPassword "NuGet key: "

    Fake.DotNet.Paket.push <| fun p ->  { p with WorkingDir = artifactsDir; ApiKey = key }
}

// --------------------------------------------------------------------------------------
// Release Scripts

#load "../paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"

let gitRelease = task "GitRelease" [] {
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion
}

let githubRelease = task "GitHubRelease" [ zip ] {
    let user =
        match Environment.environVarOrNone "github-user" with
        | Some s -> s
        | _ -> UserInput.getUserInput "GitHub Username: "
    let pw =
        match Environment.environVarOrNone "github-pw" with
        | Some s -> s
        | _ -> UserInput.getUserPassword "GitHub Password or Token: "

    // release on github
    Octokit.createClient user pw
    |> Octokit.createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> Octokit.uploadFile zipPath
    |> Octokit.releaseDraft
    |> Async.RunSynchronously
}

let pack = task "Pack" [] {
    Fake.ILMergeHelper.ILMerge
        (fun p ->
            { p with
                Libraries = !! (appBinDir </> "*.dll")
                SearchDirectories = [appBinDir]
                ToolPath = rootDir </> "packages" </> "ILRepack" </> "tools" </> "ilrepack.exe"
            })
        (artifactsDir </> "merged.exe")
        (appBinDir </> "stidgen.exe")
}

// --------------------------------------------------------------------------------------
// Empty targets for readability

let packages = EmptyTask "Packages" [ zip; nuget ]
let releaseTask = EmptyTask "Release" [ githubRelease; publishNuget ]
let ci = EmptyTask "CI" [ clean; runTests; packages ]

// --------------------------------------------------------------------------------------
// Go! Go! Go!

RunTaskOrDefault (EmptyTask "Default" [ runTests ])

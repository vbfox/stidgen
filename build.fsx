﻿#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
#if MONO
#else
#load "packages/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif

let nunitPath = @"packages\NUnit.Runners\tools"
let binDir = __SOURCE_DIRECTORY__ </> "bin"
let testsDir = binDir </> "tests"
let appBinDir = binDir </> "BlackFox.Stidgen"

let project = "stidgen"
let summary = "Strongly Typed ID type Generator"
let solutionFile  = __SOURCE_DIRECTORY__ </> project + ".sln"
let testAssemblies = __SOURCE_DIRECTORY__ </> "tests/**/bin/Release/*.Tests.dll"
let sourceProjects = __SOURCE_DIRECTORY__ </> "src/**/*.??proj"


/// The profile where the project is posted
let gitOwner = "vbfox" 
let gitHome = "https://github.com/" + gitOwner

/// The name of the project on GitHub
let gitName = "stidgen"

/// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

// --------------------------------------------------------------------------------------
// Build steps
// --------------------------------------------------------------------------------------

// Parameter helpers to be able to get parameters from either command line or environment
let getParamOrDefault name value = environVarOrDefault name <| getBuildParamOrDefault name value

let getParam name = 
    let str = getParamOrDefault name ""
    match str with
        | "" -> None
        | _ -> Some(str)

// Read additional information from the release notes document
let release = LoadReleaseNotes "Release Notes.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|) (projFileName:string) = 
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" <| fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

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
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        )

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" <| fun _ ->
    CreateDir binDir
    !! sourceProjects
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin/Release", binDir </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" <| fun _ -> CleanDir binDir

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" <| fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" <| fun _ ->
    !! testAssemblies
      |> NUnit (fun p ->
          {p with
             ToolPath = nunitPath
             DisableShadowCopy = true
             TimeOut = TimeSpan.FromMinutes 20.
             OutputFile = testsDir + "TestResults.xml" })

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries http://ctaggart.github.io/SourceLink/

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName
    !! "src/**/*.??proj"
    |> Seq.iter (fun projFile ->
        let proj = VsProj.LoadRelease projFile 
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl
    )
)

#endif

// --------------------------------------------------------------------------------------
// Build a Zip package

let zipPath = binDir </> (sprintf "%s-%s.zip" project release.NugetVersion)

Target "Zip" (fun _ ->
    let comment = sprintf "%s v%s" project release.NugetVersion
    let files =
        !! (appBinDir </> "*.dll")
        ++ (appBinDir </> "*.pdb")
        ++ (appBinDir </> "*.exe")
    ZipHelper.CreateZip appBinDir zipPath comment 7 false files
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" <| fun _ ->
    Paket.Pack <| fun p -> 
        { p with
            OutputPath = binDir
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes}

Target "PublishNuget" <| fun _ ->
    let key =
        match getParam "nuget-key" with
        | Some(key) -> key
        | None -> getUserPassword "NuGet key: "
        
    Paket.Push <| fun p ->  { p with WorkingDir = binDir; ApiKey = key }

// --------------------------------------------------------------------------------------
// Release Scripts

#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"

Target "GitHubRelease" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion
    
    // release on github
    Octokit.createClient user pw
    |> Octokit.createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    |> Octokit.uploadFile zipPath    
    |> Octokit.releaseDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Empty targets for readability

Target "Default" <| fun _ -> trace "Default target executed"
Target "Release" <| fun _ -> trace "Release target executed"

Target "Paket" <| fun _ -> trace "Paket should have been executed"

// --------------------------------------------------------------------------------------
// Target dependencies

"Clean"
    ==> "AssemblyInfo"
    ==> "Build"
    ==> "CopyBinaries"
    ==> "RunTests"
    ==> "Default"

let finalBinaries =
    "Default"
#if MONO
#else
    =?> ("SourceLink", Pdbstr.tryFind().IsSome )
#endif

finalBinaries
    ==> "Zip"
    ==> "GitHubRelease"
    ==> "Release"
    
finalBinaries
    ==> "NuGet"
    ==> "PublishNuget"
    ==> "Release"

RunTargetOrDefault "Default"
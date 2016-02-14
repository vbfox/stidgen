#r @"../packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.Testing.NUnit3
open System
open System.IO
#if MONO
#else
#load "../packages/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif

let configuration = "Release"
let rootDir = Path.GetFullPath(__SOURCE_DIRECTORY__ </> "..")
let artifactsDir = rootDir </> "artifacts"
let nunitPath = rootDir </> @"packages" </> "NUnit.Console" </> "tools" </> "nunit3-console.exe"
let appBinDir = artifactsDir </> "bin" </> "BlackFox.Stidgen" </> configuration

let project = "stidgen"
let summary = "Strongly Typed ID type Generator"
let solutionFile  = rootDir </> project + ".sln"
let testAssemblies = artifactsDir </> "bin" </> "*.Tests" </> configuration </> "*.Tests.dll"
let sourceProjects = rootDir </> "src/**/*.??proj"


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
        | Csproj -> CreateCSharpAssemblyInfo (folderName </> "Properties" </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo (folderName </> "My Project" </> "AssemblyInfo.vb") attributes
        )

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" <| fun _ ->
    CleanDir artifactsDir
    
    !! solutionFile
    |> MSBuildRelease "" "Clean"
    |> ignore

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" <| fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

module AppveyorEx =
    type TestResultsType =
        | MsTest
        | Xunit
        | NUnit
        | NUnit3
        | JUnit

    /// Uploads a test result file to make them visible in Test tab of the build console.
    let UploadTestResult (testResultsType : TestResultsType) file =
        if buildServer = BuildServer.AppVeyor then
            let resultsType = (sprintf "%A" testResultsType).ToLower()
            let url = sprintf "https://ci.appveyor.com/api/testresults/%s/%s" resultsType AppVeyor.AppVeyorEnvironment.JobId
            use wc = new System.Net.WebClient()
            try
                wc.UploadFile(url, file) |> ignore
                printfn "Successfully uploaded test results %s" file
            with
            | ex -> printfn "An error occurred while uploading %s:\r\n%O" file ex

    let private sendToAppVeyor args = 
        ExecProcess (fun info -> 
            info.FileName <- "appveyor"
            info.Arguments <- args) (System.TimeSpan.MaxValue)
        |> ignore
        
    /// Set environment variable
    let SetVariable name value =
        sendToAppVeyor <| sprintf "SetVariable -Name \"%s\" -Value \"%s\"" name value

    type ArtifactType = Auto | WebDeployPackage

    type PushArtifactParams =
        {
            Path: string
            FileName: string
            DeploymentName: string
            Type: ArtifactType option
        }
        
    let defaultPushArtifactParams =
        {
            Path = ""
            FileName = ""
            DeploymentName = ""
            Type = None
        }

    let private appendArgIfNotNullOrEmpty value name builder =
        if (isNotNullOrEmpty value) then
            appendWithoutQuotes (sprintf "-%s \"%s\"" name value) builder
        else
            builder

    let PushArtifactEx (setParams : PushArtifactParams -> PushArtifactParams) =
        let parameters = setParams defaultPushArtifactParams
        new System.Text.StringBuilder()
        |> append "PushArtifact"
        |> append parameters.Path
        |> appendArgIfNotNullOrEmpty parameters.FileName "FileName"
        |> appendArgIfNotNullOrEmpty parameters.DeploymentName "DeploymentName"
        |> appendIfSome parameters.Type (sprintf "-Type \"%A\"")
        |> toText
        |> sendToAppVeyor

    let inline PushArtifact path =
        PushArtifactEx (fun p ->
            { p with
                Path = path
                FileName = Path.GetFileName(path)
            })

Target "RunTests" <| fun _ ->
    let testResults = artifactsDir</> "TestResults.xml"
    !! testAssemblies
      |> NUnit3 (fun p ->
          {p with
             ToolPath = nunitPath
             TimeOut = TimeSpan.FromMinutes 20.
             DisposeRunners = true
             ResultSpecs = [testResults] })

    AppveyorEx.UploadTestResult AppveyorEx.NUnit3 testResults
#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries http://ctaggart.github.io/SourceLink/

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName
    tracefn "SourceLink base URL: %s" baseUrl 
    
    !! sourceProjects
    |> Seq.iter (fun projFile ->
        let projectName = Path.GetFileNameWithoutExtension projFile
        let proj = VsProj.LoadRelease projFile
        tracefn "Generating SourceLink for %s on pdb: %s" projectName proj.OutputFilePdb
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb rootDir baseUrl
    )
)

#endif

// --------------------------------------------------------------------------------------
// Build a Zip package

let zipPath = artifactsDir </> (sprintf "%s-%s.zip" project release.NugetVersion)

Target "Zip" (fun _ ->
    let comment = sprintf "%s v%s" project release.NugetVersion
    let files =
        !! (appBinDir </> "*.dll")
        ++ (appBinDir </> "*.config")
        ++ (appBinDir </> "*.exe")
    ZipHelper.CreateZip appBinDir zipPath comment 9 false files
    
    AppveyorEx.PushArtifactEx (fun p ->
        { p with
            Path = zipPath
            FileName = Path.GetFileName(zipPath)
            DeploymentName = "Binaries"
        })
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" <| fun _ ->
    Paket.Pack <| fun p -> 
        { p with
            OutputPath = artifactsDir
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes
            WorkingDir = appBinDir }
            
    !! (artifactsDir </> "*.nupkg")
    |> Seq.iter AppveyorEx.PushArtifact

Target "PublishNuget" <| fun _ ->
    let key =
        match getParam "nuget-key" with
        | Some(key) -> key
        | None -> getUserPassword "NuGet key: "
        
    Paket.Push <| fun p ->  { p with WorkingDir = artifactsDir; ApiKey = key }

// --------------------------------------------------------------------------------------
// Release Scripts

#load "../paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"

Target "GitRelease" (fun _ ->
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
)

Target "GitHubRelease" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "GitHub Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "GitHub Password or Token: "

    // release on github
    Octokit.createClient user pw
    |> Octokit.createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    |> Octokit.uploadFile zipPath    
    |> Octokit.releaseDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Empty targets for readability

Target "Default" DoNothing
Target "Release" DoNothing
Target "Paket" DoNothing
Target "CI" DoNothing

// --------------------------------------------------------------------------------------
// Target dependencies

"Clean"
    ?=> "AssemblyInfo"
    ==> "Build"
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

"Clean" ==> "CI"    
"Zip" ==> "CI"
"NuGet" ==> "CI"
    
finalBinaries
    ==> "NuGet"
    ==> "PublishNuget"
    ==> "Release"

RunTargetOrDefault "Default"
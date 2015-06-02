#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO

let nunitPath = @"packages\NUnit.Runners\tools"
let binDir = __SOURCE_DIRECTORY__ @@ "bin"
let testsDir = Path.Combine(binDir, "tests")

let project = "stidgen"
let summary = "Strongly Typed ID type Generator"
let solutionFile  = __SOURCE_DIRECTORY__ @@ project + ".sln"
let testAssemblies = __SOURCE_DIRECTORY__ @@ "tests/**/bin/Release/*.Tests.dll"
let sourceProjects = __SOURCE_DIRECTORY__ @@ "src/**/*.??proj"

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
        | Fsproj -> CreateFSharpAssemblyInfo (folderName @@ "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName @@ "Properties") @@ "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName @@ "My Project") @@ "AssemblyInfo.vb") attributes
        )

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" <| fun _ ->
    CreateDir binDir
    !! sourceProjects
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin/Release", binDir @@ (System.IO.Path.GetFileNameWithoutExtension f)))
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

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" <| fun _ ->
    Paket.Pack <| fun p -> 
        { p with
            OutputPath = binDir
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes}

Target "PublishNuget" <| fun _ ->
    let key = getParam "nugetkey"
    match key with
    | Some(key) -> Paket.Push <| fun p ->  { p with WorkingDir = binDir; ApiKey = key }
    | None -> failwith "No API key configured, use nugetkey=value or specify it as an environment variable"

// --------------------------------------------------------------------------------------
// Empty targets for readability

Target "Default" <| fun _ -> trace "Default target executed"

Target "Paket" <| fun _ -> trace "Paket should have been executed"

// --------------------------------------------------------------------------------------
// Target dependencies

"Clean"
    ==> "AssemblyInfo"
    ==> "Build"
    ==> "CopyBinaries"
    ==> "Default"

"Default"
    ==> "NuGet"
    ==> "PublishNuget"

"CopyBinaries"
    ==> "RunTests"

RunTargetOrDefault "Default"
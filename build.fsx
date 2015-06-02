#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO

let nunitPath = @"packages\NUnit.Runners\tools"
let binDir = Path.Combine(__SOURCE_DIRECTORY__, "bin")
let testsDir = Path.Combine(binDir, "tests")

let project = "stidgen"
let summary = "Strongly Typed ID type Generator"
let solutionFile  = project + ".sln"
let testAssemblies = "tests/**/bin/Release/*.Tests.dll"

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
Target "AssemblyInfo" (fun _ ->
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

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName @@ "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName @@ "Properties") @@ "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName @@ "My Project") @@ "AssemblyInfo.vb") attributes
        )
)

// Copies binaries from default VS location to exepcted bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" <|fun _ ->
    CreateDir binDir
    !! "src/**/*.??proj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin/Release", binDir @@ (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))

Target "Build" <|fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore

Target "RunTests" <| fun _ ->
    !! testAssemblies
      |> NUnit (fun p ->
          {p with
             ToolPath = nunitPath
             DisableShadowCopy = true
             TimeOut = TimeSpan.FromMinutes 20.
             OutputFile = testsDir + "TestResults.xml" })

Target "Package" <| fun _ ->
    ()

Target "Clean" <| fun _ -> CleanDir binDir

Target "Default" <|fun _ -> trace "Default target executed"

Target "Paket" <| fun _ -> trace "Paket should have been executed"

"Clean"
    ==> "AssemblyInfo"
    ==> "Build"
    ==> "CopyBinaries"
    ==> "Default"

"CopyBinaries"
    ==> "RunTests"

RunTargetOrDefault "Default"
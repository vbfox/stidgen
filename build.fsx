#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO

let nunitPath = @"packages\NUnit.Runners\tools"
let binDir = Path.Combine(__SOURCE_DIRECTORY__, "bin")
let buildDir = Path.Combine(binDir, "build")
let testsDir = Path.Combine(binDir, "tests")

let solutionFile  = "stidgen.sln"

// Copies binaries from default VS location to exepcted bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin/Release", binDir @@ (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))
)

Target "Build" (fun _ ->
    CreateDir buildDir
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

Target "RunTests" (fun _ ->
    !! (testsDir + "/UnitTests.dll")
      |> NUnit (fun p ->
          {p with
             ToolPath = nunitPath
             DisableShadowCopy = true;
             OutputFile = testsDir + "TestResults.xml" })
)

Target "Clean" (fun _ ->
    CleanDir binDir
)

Target "Default" (fun _ ->
    trace "Default target executed"
)

Target "Paket" (fun _ ->
    trace "Paket should have been executed"
)

"Clean"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "RunTests"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
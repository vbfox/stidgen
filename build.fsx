#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO

let nunitPath = @"packages\NUnit.Runners\tools"
let binDir = Path.Combine(__SOURCE_DIRECTORY__, "bin")
let buildDir = Path.Combine(binDir, "build")
let testsDir = Path.Combine(binDir, "tests")

Target "BuildApp" (fun _ ->
    CreateDir buildDir
    !! "stidgen/stidgen.fsproj"
      |> MSBuildRelease buildDir "Build"
      |> Log "Build Output: "
)

Target "BuildTests" (fun _ ->
    CreateDir testsDir
    !! "UnitTests/UnitTests.fsproj"
      |> MSBuildRelease testsDir "Build"
      |> Log "Tests Build Output: "
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
  ==> "BuildApp"
  ==> "BuildTests"
  ==> "RunTests"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
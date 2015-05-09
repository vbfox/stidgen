#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO

let nunitPath = @"packages\NUnit.Runners\tools"
let binDir = Path.Combine(__SOURCE_DIRECTORY__, "bin")
let testsDir = Path.Combine(binDir, "tests")

let solutionFile  = "stidgen.sln"
let testAssemblies = "tests/**/bin/Release/*.Tests.dll"

// Copies binaries from default VS location to exepcted bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" (fun _ ->
    CreateDir binDir
    !! "src/**/*.??proj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin/Release", binDir @@ (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))
)

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

Target "RunTests" (fun _ ->
    !! testAssemblies
      |> NUnit (fun p ->
          {p with
             ToolPath = nunitPath
             DisableShadowCopy = true
             TimeOut = TimeSpan.FromMinutes 20.
             OutputFile = testsDir + "TestResults.xml" })
)

Target "Clean" (fun _ ->
    CleanDir binDir
)

let escapeArg argText =
    let empty = System.String.IsNullOrEmpty(argText)
    let notInteresting = argText.IndexOfAny([|' ';'\t';'\n';'\v';'"'|]) = -1
    if empty || notInteresting then
        argText
    else
        let chars = seq {
            yield '"'
            let numberBackslashes = ref 0
            for c in argText do
                match c with
                | '\\' -> numberBackslashes := !numberBackslashes + 1
                | '"' ->
                    for i=1 to !numberBackslashes * 2 + 1 do yield '\\'
                    numberBackslashes := 0
                    yield '"'
                | c ->
                    for i=1 to !numberBackslashes do yield '\\'
                    numberBackslashes := 0
                    yield c

            for i=1 to !numberBackslashes * 2 do yield '\\'
            yield '"'
        }
        new System.String(chars |> Array.ofSeq)

let escapeArgs args =
    let escaped = args |> List.map escapeArg |> Seq.ofList
    System.String.Join(" ", escaped)

Target "Merge" (fun _ -> 
    let exeBinDir = binDir @@ "BlackFox.Stidgen"
    let mainExe = exeBinDir @@ "stidgen.exe"
    let ilRepack = Path.Combine(__SOURCE_DIRECTORY__, "packages", "ILRepack", "tools", "ILRepack.exe")
    let dlls = (!! (exeBinDir + "/*.dll")) :> string seq |> List.ofSeq
    let args = [
        "/out:" + (exeBinDir @@ "stidgen.pack.exe")
        mainExe
    ]
    (*
    let args = List.concat [args;dlls]
    printfn "%s" (escapeArgs args)
    directExec (fun info -> 
        info.FileName <- ilRepack
        info.Arguments <- escapeArgs args
        ) |> ignore
        *)
    let ilMerge = Path.Combine(__SOURCE_DIRECTORY__, "packages", "ilmerge", "tools", "ILMerge.exe")
    ILMergeHelper.ILMerge (fun p -> 
        { p with Libraries = [exeBinDir@@"FSharp.Core.dll"]; ToolPath = ilMerge; TargetKind = TargetKind.Exe}
        ) (exeBinDir @@ "stidgen.ilmerge.exe") mainExe
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
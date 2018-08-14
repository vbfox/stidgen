module BlackFox.ExpectoDotNetCli

open System
open Fake.Core
open Fake.DotNet.Testing
open Fake.Testing.Common

type private ResolvedRunMode = | ResolvedDirect | ResolvedDotNet

let private getRunMode (assembly: string) =
    match System.IO.Path.GetExtension(assembly).ToLowerInvariant() with
    | ".dll" -> ResolvedDotNet
    | ".exe" -> ResolvedDirect
    | ext ->
        failwithf "Unable to find a way to run expecto test executable with extension %s" ext

let private runAssembly (expectoParams: Expecto.Params) testAssembly =
    let fakeStartInfo  =
        let runMode = getRunMode testAssembly
        let workingDir =
            if String.isNotNullOrEmpty expectoParams.WorkingDirectory
            then expectoParams.WorkingDirectory else Fake.IO.Path.getDirectory testAssembly
        let fileName, argsString =
            match runMode with
            | ResolvedDotNet ->
                "dotnet", sprintf "\"%s\" %O" testAssembly expectoParams
            | ResolvedDirect ->
                testAssembly, string expectoParams
        (fun (info: ProcStartInfo) ->
            { info with
                FileName = fileName
                Arguments = argsString
                WorkingDirectory = workingDir } )

    let exitCode = Process.execSimple fakeStartInfo TimeSpan.MaxValue
    testAssembly, exitCode

let run (setParams : Expecto.Params -> Expecto.Params) (assemblies : string seq) =
    let details = assemblies |> String.separated ", "
    use __ = Trace.traceTask "Expecto" details

    let expectoParams = setParams Expecto.Params.DefaultParams

    let res =
        assemblies
        |> Seq.map (runAssembly expectoParams)
        |> Seq.filter( snd >> (<>) 0)
        |> Seq.toList

    match res with
    | [] -> ()
    | failedAssemblies ->
        failedAssemblies
        |> List.map (fun (testAssembly,exitCode) ->
            sprintf "Expecto test of assembly '%s' failed. Process finished with exit code %d." testAssembly exitCode )
        |> String.concat System.Environment.NewLine
        |> FailedTestsException |> raise
    __.MarkSuccess()

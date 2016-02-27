﻿module BlackFox.Stidgen.Program

open BlackFox

type RunResults = FileGeneration.GenerationResult list

let run fileGlobs =
    let baseDir = System.Environment.CurrentDirectory
    
    [
        for glob in fileGlobs do
            let outputs = [
                for file in Fake.Globbing.search baseDir glob do
                    yield FileGeneration.generateToFiles file
            ]
            yield glob, outputs
    ]

let private printUsage () =
    sprintf "^[yellow]Usage^[reset]:" |> coloredWriteLine
    sprintf "    ^[white]stidgen^[reset] fileGlob ^[darkgray][^[reset]fileGlob ...^[darkgray]]^[reset]" |> coloredWriteLine

let private printSuccessResult (result:FileGeneration.GenerationResult) =
    for file in result.Files do
        sprintf "Generated in ^[yellow]%s^[reset]:" file.FileName |> coloredWriteLine
        for generated in file.IdTypes do
            sprintf "  ^[darkgray]*^[white] %s" generated.Name |> coloredWriteLine
    printfn ""

let private printErrorResult (result:FileGeneration.GenerationResult) = 
    sprintf "^[red]Errors in ^[yellow]%s^[red]:" result.Configuration.Path.Value |> coloredWriteLine
    for error in result.Configuration.Errors do
        sprintf "^[red]\t%O" error |> coloredWriteLine
        sprintf "^[red]\t\tContent: %s" error.Line.Text |> coloredWriteLine
    printfn ""

let private printResult (results: (string*RunResults) list) =
    for glob, results in results do
        if results.IsEmpty then
            sprintf "^[red]No files found for '^[yellow]%s^[red]'" glob |> coloredWriteLine
            printfn ""
        else
            for result in results do
                if result.HasErrors() then
                    printErrorResult result
                else
                    printSuccessResult result

let private getExitCode (result: (string*RunResults) list) = 
    let hasErrors = result |> List.collect snd |> List.exists(fun r -> r.HasErrors())
    let noFiles = result |> List.collect snd |> List.isEmpty
    if hasErrors || noFiles then
        -1
    else
        0

[<EntryPoint>]
let main argv = 
    if argv.Length > 0 then
        let result = run argv
        printResult result
        getExitCode result
    else
        printUsage ()
        0
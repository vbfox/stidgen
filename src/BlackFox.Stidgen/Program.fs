module BlackFox.Stidgen.Program

open BlackFox
open BlackFox.ColoredPrintf
open BlackFox.Stidgen.Control

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
    colorprintfn "$yellow[Usage]:"
    colorprintfn "    $white[stidgen] fileGlob $darkgray[[]fileGlob ...$darkgray[\]]"

let private printSuccessResult (result:FileGeneration.GenerationResult) =
    for file in result.Files do
        colorprintfn "Generated in $yellow[%s]:" file.FileName
        for generated in file.IdTypes do
            colorprintfn "  $darkgray[*] $white[%s]" generated.Name
    printfn ""

let private printErrorResult (result:FileGeneration.GenerationResult) = 
    colorprintfn "$red[Errors in $yellow[%s]:" result.Configuration.Path.Value

    match result.Configuration.Result with
    | Success _ -> failwith "Unexpected success"
    | Failure (ConfigurationParser.InvalidUnderlyingTypes(types)) ->
        colorprintfn "$red[\tUnable to resolve the following underlying types:"
        for t in types do
            let underlying = System.String.Join(".", t.UnderlyingType)
            let targetType = System.String.Join(".", t.FullName)
            colorprintfn "\t $darkred[*] $red[%s (In %s)]" underlying targetType
    | Failure (ConfigurationParser.ParseError(msg, _)) ->
        colorprintfn "$red[\t%s" (msg.Replace("\r\n", "\r\n\t"))

let private printResult (results: (string*RunResults) list) =
    for glob, results in results do
        if results.IsEmpty then
            colorprintfn "$red[No files found for '$yellow[%s]'" glob
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
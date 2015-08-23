module BlackFox.Stidgen.Program

type runResult = (string  *ConfigurationParser.ParseError list) list

let run (fileGlobs : string seq) : runResult =
    let baseDir = System.Environment.CurrentDirectory
    
    fileGlobs
        |> Seq.collect (fun glob -> Fake.Globbing.search baseDir glob)
        |> Seq.map (fun f -> f, FileGeneration.generateToFiles f)
        |> List.ofSeq

let isSuccess (result: runResult) = 
    let errorCount = result |> Seq.collect snd |> Seq.length
    errorCount = 0

let private printUsage () =
    printfn "Usage:"
    printfn "    stidgen fileName"

let private printResult (result: runResult) =
    for file, errors in result do
        match errors with
        | [] -> ()
        | errors ->
            eprintfn "In %s:" file
            for error in errors do
                eprintfn "\t%O" error
                eprintfn "\t\tContent: %s" error.Line.Text
            eprintfn ""

let private getExitCode (result: runResult) = 
    match isSuccess result with
    | true -> 0
    | false -> -1

[<EntryPoint>]
let main argv = 
    if argv.Length > 0 then
        let result = run argv
        printResult result
        getExitCode result
    else
        printUsage ()
        0
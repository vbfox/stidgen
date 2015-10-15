module BlackFox.Stidgen.Program

type RunResults = FileGeneration.GenerationResult list

let run (fileGlobs : string seq) : RunResults =
    let baseDir = System.Environment.CurrentDirectory
    
    fileGlobs
        |> Seq.collect (fun glob -> Fake.Globbing.search baseDir glob)
        |> Seq.map FileGeneration.generateToFiles
        |> List.ofSeq

let private printUsage () =
    printfn "Usage:"
    printfn "    stidgen fileGlob [fileGlob ...]"

let private printResult (results: RunResults) =
    for result in results do
        if result.HasErrors() then
            eprintfn "Errors in %s:" result.Configuration.Path.Value
            for error in result.Configuration.Errors do
                eprintfn "\t%O" error
                eprintfn "\t\tContent: %s" error.Line.Text
            eprintfn ""
        else
            for file in result.Files do
                printfn "Generated in %s:" file.FileName
                for generated in file.IdTypes do
                    printfn "\t* %s" generated.Name
            printfn ""

let private getExitCode (result: RunResults) = 
    if result |> Seq.exists(fun r -> r.HasErrors()) then
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
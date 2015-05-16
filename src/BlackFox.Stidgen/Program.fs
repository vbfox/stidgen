module BlackFox.Stidgen.Program

let printUsage () =
    printf "Usage:"
    printf "    stidgen fileName"

[<EntryPoint>]
let main argv = 
    if argv.Length = 1 then
        match FileGeneration.generateToFiles argv.[0] with
        | [] -> 0
        | errors ->
            for error in errors do
                printfn "%O" error
                printfn "\tContent: %s" error.Line.Text
            -1
    else
        printUsage ()
        0
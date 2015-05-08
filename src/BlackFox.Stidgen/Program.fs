module BlackFox.Stidgen.Program

let printUsage () =
    printf "Usage:"
    printf "    stidgen fileName"

[<EntryPoint>]
let main argv = 
    if argv.Length = 1 then
        FileGeneration.generateToFiles argv.[0]
    else
        printUsage ()

    0
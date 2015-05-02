module BlackFox.Stidgen.Program

open BlackFox.Stidgen.Description
open BlackFox.Stidgen.CsharpGeneration
open System.IO

let getIdType () =
    makeIdFromType<string> (fun i ->
            { i with
                Name = "TestId"
                Namespace = "BlackFox.TestIdGeneration"
                EqualsUnderlying = true
            }
        )

let writeToFile fileName idType =
    let text = idTypeToString idType
    File.WriteAllText(fileName, text)

let printOnConsole idType = 
    let text = idTypeToString idType
    printf "%s" text
    System.Console.ReadLine() |> ignore

[<EntryPoint>]
let main argv = 
    let idType = getIdType ()
    if argv.Length > 0 then
        writeToFile argv.[0] idType
    else
        printOnConsole idType

    0

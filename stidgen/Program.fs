module BlackFox.Stidgen.Program

open BlackFox.Stidgen.Description
open BlackFox.Stidgen.CsharpGeneration

[<EntryPoint>]
let main argv = 
    // public BlackFox.Tests.TestId : string { Value }
    let idType = makeIdType<string> (fun i ->
            { i with
                Name = "TestId"
                Namespace = "BlackFox.TestIdGeneration"
            }
        )

    printf "%s" (idTypeToString idType)

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code

module BlackFox.Stidgen.Program

open BlackFox.Stidgen.Description
open BlackFox.Stidgen.CsharpGeneration

[<EntryPoint>]
let main argv = 
    // public BlackFox.Tests.TestId : string { Value }
    let idType : IdType =
        {
            Name = "TestId";
            Namespace = "BlackFox.Tests"
            Type = typedefof<string>;
            ValueProperty = "Value"
            Visibility = Public
        }
    printf "%s" (gen idType)
    System.Console.ReadLine()
    0 // return an integer exit code

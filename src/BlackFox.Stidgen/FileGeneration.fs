module BlackFox.Stidgen.FileGeneration

open BlackFox.Stidgen.Description
open System
open System.IO

let private normalizeFileName (s:string) = 
    let invalid = Path.GetInvalidFileNameChars()
    let newChars = 
        s
        |> Seq.map (fun c -> if invalid |> Seq.exists ((=) c) then '_' else c)
        |> Array.ofSeq
    new String (newChars)

let private makePath configurationPath (idType:IdType) =
    let folder = Path.GetDirectoryName(configurationPath)
    let fileName =
        match idType.FileName with
        | Some(fileName) -> fileName
        | Option.None -> (idType.Name |> normalizeFileName) + ".cs"

    Path.Combine(folder, fileName)

let private generateToFile configurationPath idType =
    let text = CsharpGeneration.idTypeToString idType
    let path = makePath configurationPath idType
    File.WriteAllText(path, text)

let generateToFiles configurationPath =
    let configurationInfo = new FileInfo(configurationPath)
    let configuration = ConfigurationParser.loadFromFile configurationInfo
    match configuration.Path with
    | Some(path) -> 
        for idType in configuration.Types do
            idType |> generateToFile path
    | _ -> failwith "Unexpected"
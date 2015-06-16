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
        match (idType.UseNameAsFileName, idType.FileName) with
        | (_, Some(fileName)) -> fileName
        | (true, _) -> (idType.Name |> normalizeFileName) + ".Generated.cs"
        | (false, _) ->
            let configFileName = Path.GetFileNameWithoutExtension(configurationPath)
            configFileName + ".Generated.cs"

    Path.Combine(folder, fileName)

let private getFilesAndContent configurationPath (idTypes : IdType list) =
    idTypes
    |> Seq.groupBy (fun t -> makePath configurationPath t)
    |> Seq.map(fun (path, typesInPath) -> (path, CsharpGeneration.idTypesToString typesInPath))

let private generateToFiles' configurationPath (idTypes : IdType list) =
    for (path, content) in getFilesAndContent configurationPath idTypes do
        File.WriteAllText(path, content)

/// Take the path of a '.stidgen' file and generate the associated ID Types
let generateToFiles configurationPath =
    let configurationInfo = new FileInfo(configurationPath)
    let configuration = ConfigurationParser.loadFromFile configurationInfo
    
    match configuration.Path with
    | Some(path) -> generateToFiles' path configuration.Types 
    | _ -> failwith "Unexpected"    
    
    configuration.Errors
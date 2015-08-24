module BlackFox.Stidgen.FileGeneration

open BlackFox.Stidgen.Description
open System
open System.IO

type GeneratedFileResult =
    {
        FileName : string
        Text : string
        IdTypes : IdType list
    }

type GenerationResult =
    {
        Configuration : ConfigurationParser.Configuration
        Files : GeneratedFileResult list
    }
with
    member x.HasErrors () =
        x.Configuration.HasErrors()

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

let private getFilesAndContentAsync configurationPath (idTypes : IdType list) =
    let generationForFile (path, typesInPath) =
        async {
            let! text = CsharpGeneration.idTypesToStringAsync typesInPath
            return {
                FileName = path
                Text = text
                IdTypes = typesInPath |> List.ofSeq
            }
        }

    idTypes
    |> Seq.groupBy (fun t -> makePath configurationPath t)
    |> Seq.map(generationForFile)
    |> Async.Parallel

let writeCsharpFiles files = 
    for fileResult in files do
        File.WriteAllText(fileResult.FileName, fileResult.Text)

/// Take the path of a '.stidgen' file and generate the associated ID Types
let generateToFiles configurationPath =
    let configurationInfo = new FileInfo(configurationPath)
    let configuration = ConfigurationParser.loadFromFile configurationInfo
    
    if configuration.HasErrors() then
        {
            Configuration = configuration
            Files = List.empty
        }
    else
        match configuration.Path with
        | Some(path) ->
            let files =
                getFilesAndContentAsync path configuration.Types
                |> Async.RunSynchronously
                |> List.ofArray
            let result = 
                {
                    Configuration = configuration
                    Files = files
                }

            if not (result.HasErrors()) then
                writeCsharpFiles result.Files

            result
        
        | _ -> failwith "Unexpected"    
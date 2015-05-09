module BlackFox.Stidgen.ConfigurationParser 
    
open BlackFox.Stidgen.Description

type LineContent =
    | TypeDefinition of visibility:string * namespace':string * name:string * underlyingType:string
    | Property of name:string * value:string

type Line =
    {
        Text : string
        Content : LineContent
    }

module private TextParser =
    let isEmpty (line: string) = System.String.IsNullOrWhiteSpace line
    let isComment (line: string) = line.Trim().StartsWith("//")

    let filterOut cond = Seq.filter (fun x -> not (cond x))

    let parseTypeDefinition (line:string) =
        let fail () = failwith ("Invalid type definition:" + line)

        match line.Split(' ') with
        | [| visibility; rest |] -> 
            match rest.Split('<') with
            | [| fullName; underlyingType |] ->
                if not (underlyingType.EndsWith(">")) then fail()
                let underlyingType = underlyingType.Substring(0, underlyingType.Length - 1)
                let lastDot = fullName.LastIndexOf('.')
                let namespace' =
                    if lastDot <> -1 then
                        fullName.Substring(0, lastDot)
                    else
                        ""
                let name = fullName.Substring(lastDot+1)
                
                TypeDefinition(visibility, namespace', name, underlyingType)

            | _ -> fail()
        | _ -> fail()

    let parseProperty (line:string) =
        let separator = line.IndexOf(':')
        match separator with
        | -1 -> failwith ("Invalid property definition:" + line)
        | _ ->
            let name = line.Substring(0, separator)
            let value = line.Substring(separator+1)
            Property(name,value)

    let parseLine (line:string) =
        let firstChar = line.[0]
        let content =
            if firstChar = ' ' || firstChar = '\t' then
                parseProperty (line.Trim())
            else
                parseTypeDefinition (line.Trim())

        { Text = line; Content = content }

    let parseLines lines = 
        lines
        |> filterOut isEmpty
        |> filterOut isComment
        |> Seq.map parseLine

type Configuration =
    {
        Path : string option
        Types : IdType list
    }

module private LineParser =
    let parseCast (text:string) =
        let lowerCasedText = text.ToLower()
        match lowerCasedText with
        | "explicit" -> Cast.Explicit
        | "implicit" -> Cast.Implicit
        | _ -> failwith (sprintf "Unknown cast type: '%s'" text)

    let parseOptionalString (text:string) =
        if System.String.IsNullOrWhiteSpace(text) then
            Option.None
        else
            Some(text)

    let addProperty' (name:string) (value:string) (idType:IdType) =
        match name with
        | "ValueProperty" -> { idType with ValueProperty = value }
        | "AllowNull" -> { idType with AllowNull = bool.Parse(value) }
        | "InternString" -> { idType with InternString = bool.Parse(value) }
        | "EqualsUnderlying" -> { idType with EqualsUnderlying = bool.Parse(value) }
        | "CastToUnderlying" -> { idType with CastToUnderlying = parseCast(value) }
        | "CastFromUnderlying" -> { idType with CastFromUnderlying = parseCast(value) }
        | "FileName" -> { idType with FileName = parseOptionalString(value) }
        | _ -> failwith(sprintf "Property '%s' isn't supported" name)

    let addProperty (content:LineContent) (idType:IdType) =
        match content with
        | Property(name, value) ->
            idType |> addProperty' (name.Trim()) (value.Trim())
        | _ -> failwith "Not a property definition"

    let parseUnderlyingType = function
        | "bool" -> typeof<bool>
        | "byte" -> typeof<byte>
        | "sbyte" -> typeof<sbyte>
        | "char" -> typeof<char>
        | "decimal" -> typeof<decimal>
        | "double" -> typeof<double>
        | "float" -> typeof<single>
        | "int" -> typeof<int>
        | "uint" -> typeof<System.UInt32>
        | "long" -> typeof<System.Int64>
        | "ulong" -> typeof<System.UInt64>
        | "object" -> typeof<obj>
        | "short" -> typeof<System.Int16>
        | "ushort" -> typeof<System.UInt16>
        | "string" -> typeof<string>
        | s ->
            let type' = System.Type.GetType(s, false)
            let type' = if type' <> null then type' else System.Type.GetType("System." + s, false)

            if type' <> null then
                type'
            else
                failwith (sprintf "Type '%s' not found." s)

    let parseVisibility = function
        | "public" -> Public
        | "internal" -> Internal
        | s -> failwith (sprintf "Visibility can't be parsed: '%s'" s)

    let typeDefinitionToType (content:LineContent) =
        match content with
        | TypeDefinition(visibility,namespace',name,underlyingType) ->
            let underlyingType = parseUnderlyingType underlyingType
            makeIdType underlyingType (fun idType ->
                { idType with
                    Name = name
                    Namespace = namespace'
                    Visibility = parseVisibility visibility
                }
            )
        | _ -> failwith "Not a type definition"

    let rec linesToIdTypes (currentType:IdType option) (remainingLines : Line list) =
        match remainingLines with
        | line :: rest ->
            match line.Content with
            | Property(_,_) -> 
                match currentType with
                | Some(idType) -> 
                    let idType = idType |> addProperty line.Content
                    linesToIdTypes (Some(idType)) rest
                | Option.None -> failwith ("Found a property line before founding any type: " + line.Text)
            | TypeDefinition(_,_,_,_) ->
                let newType = typeDefinitionToType line.Content
                match currentType with
                | Some(idType) -> idType :: linesToIdTypes (Some(newType)) rest
                | Option.None -> linesToIdTypes (Some(newType)) rest
        | [] -> 
            match currentType with
            | Some(idType) -> [idType]
            | Option.None -> []

    let linesToConfiguration (lines:Line seq) : Configuration =
        let lines = lines |> List.ofSeq
        let types = linesToIdTypes Option.None lines
        { Path = Option.None; Types = types}

let loadFromLines (lines:string seq) =
    lines
    |> TextParser.parseLines
    |> LineParser.linesToConfiguration

open System.IO

let loadFromTextReader (reader:TextReader) = 
    let lines = seq {
        let line = ref (reader.ReadLine())
        while !line <> null do
            yield !line
            line := reader.ReadLine()
    }
    loadFromLines lines

let loadFromStream (stream:Stream) =
    use reader = new StreamReader(stream)
    loadFromTextReader reader

let loadFromFile (file:FileInfo) =
    use reader = new FileStream(file.FullName, FileMode.Open)
    let configuration = loadFromStream reader
    { configuration with Path = Some(file.FullName) }
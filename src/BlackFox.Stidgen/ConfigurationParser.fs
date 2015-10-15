module BlackFox.Stidgen.ConfigurationParser 
    
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.Control

type LineContent =
    | TypeDefinition of visibility:string * namespace':string * name:string * underlyingType:string
    | Property of name:string * value:string

type Line =
    {
        Number : int
        Text : string
    }
    override x.ToString () = sprintf "%i: %s" x.Number x.Text

type ParsedLine =
    {
        Line : Line
        Content : LineContent
    }
    override x.ToString () = x.Line.ToString()

type ErrorText = string
type ParseError =
    {
        Line : Line
        ErrorText : ErrorText
        IsInvalidType : bool
    }
    static member fromString text line  = { Line = line; ErrorText = text; IsInvalidType = false }
    override x.ToString () = sprintf "Error line %i: %s" x.Line.Number x.ErrorText

module private TextParser =
    let isEmpty (line: string) = System.String.IsNullOrWhiteSpace line
    let isComment (line: string) = line.Trim().StartsWith("//")

    let filterOut cond = Seq.filter (fun (x:Line) -> not (cond x.Text))

    let parseTypeDefinition (line:string) =
        let fail details =
            Failure (sprintf "Invalid type definition, should %s like 'public MyType<int>'" details)

        match line.Split(' ') with
        | [| visibility; rest |] -> 
            match rest.Split('<') with
            | [| fullName; underlyingType |] ->
                match underlyingType with
                | _ when underlyingType.Length = 0 -> fail "have a non-empty underlying type"
                | _ when fullName.Length = 0 -> fail "have a non-empty type name"
                | _ when not (underlyingType.EndsWith(">")) -> fail "end with >"
                | _ ->
                    let underlyingType = underlyingType.Substring(0, underlyingType.Length - 1)
                    let lastDot = fullName.LastIndexOf('.')
                    let namespace' =
                        if lastDot <> -1 then
                            fullName.Substring(0, lastDot)
                        else
                            ""
                    let name = fullName.Substring(lastDot+1)
                
                    Success (TypeDefinition(visibility, namespace', name, underlyingType))

            | _ -> fail "contain an underlying type between <>"
        | _ -> fail "contain one space"

    let parseProperty (line:string) =
        let separator = line.IndexOf(':')
        match separator with
        | -1 -> Failure "Invalid property definition, should be 'name:value'"
        | _ ->
            let name = line.Substring(0, separator)
            let value = line.Substring(separator+1)
            Success (Property(name,value))

    let augmentChoice line isType contentOrErrorText =
        match contentOrErrorText with
        | Success(content) -> Success { Line = line; Content = content }
        | Failure(errorText) -> Failure { Line = line; ErrorText = errorText; IsInvalidType = isType }

    let parseLine (line:Line) =
        let text = line.Text
        let firstChar = text.[0]

        if System.Char.IsWhiteSpace(firstChar) then
            parseProperty (text.Trim()) |> augmentChoice line false
        else
            parseTypeDefinition (text.Trim()) |> augmentChoice line true

    let parseLines lines = 
        lines
        |> Seq.mapi (fun i l -> { Number = i+1; Text = l})
        |> filterOut isEmpty
        |> filterOut isComment
        |> Seq.map parseLine

type Configuration =
    {
        /// Path of the '.stidgen' file
        Path : string option
        
        /// Types correctly readed
        Types : IdType list
        
        /// Parse errors
        Errors : ParseError list
    }
with
    member x.HasErrors () =
        x.Errors
            |> List.length
            |> (<>) 0

module private LineParser =
    let parseCast (text:string) = result {
        let lowerCasedText = text.ToLower()
        match lowerCasedText with
        | "explicit" -> return Cast.Explicit
        | "implicit" -> return Cast.Implicit
        | _ -> return! Failure (sprintf "Unknown cast type: '%s'" text)
    }

    let parseOptionalString (text:string) =
        if System.String.IsNullOrWhiteSpace(text) then
            Option.None
        else
            Some(text)

    let parseBool (text:string) =
        match bool.TryParse(text) with
        | (true, parsed) -> Success parsed
        | (false, _) -> Failure (sprintf "The value '%s' isn't a boolean value" text)

    let addProperty' (name:string) (value:string) (idType:IdType) = result {
        match name with
        | "ValueProperty" -> return { idType with ValueProperty = value }
        | "AllowNull" ->
            let! b = parseBool value
            return { idType with AllowNull = b }
        | "InternString" ->
            let! b = parseBool value
            return { idType with InternString = b }
        | "EqualsUnderlying" ->
            let! b = parseBool value
            return { idType with EqualsUnderlying = b }
        | "CastToUnderlying" ->
            let! cast = parseCast(value)
            return { idType with CastToUnderlying = cast }
        | "CastFromUnderlying" ->
            let! cast = parseCast(value)
            return { idType with CastFromUnderlying = cast }
        | "UseNameAsFileName" ->
            let! b = parseBool value
            return { idType with UseNameAsFileName = b }
        | "FileName" -> return { idType with FileName = parseOptionalString(value) }
        | "ProtobufnetSerializable" ->
            let! b = parseBool value
            return { idType with ProtobufnetSerializable = b }
        | "DataContractSerializable" ->
            let! b = parseBool value
            return { idType with DataContractSerializable = b }
        | _ -> return! Failure (sprintf "Property '%s' isn't supported" name)
    }

    let addProperty (content:LineContent) (idType:IdType) = result {
        match content with
        | Property(name, value) ->
            let! newType = idType |> addProperty' (name.Trim()) (value.Trim())
            return newType
        | _ -> return! Failure "Not a property definition"
    }

    let parseUnderlyingType typeName = result {
        match typeName with
        | "bool" -> return typeof<bool>
        | "byte" -> return typeof<byte>
        | "sbyte" -> return typeof<sbyte>
        | "char" -> return typeof<char>
        | "decimal" -> return typeof<decimal>
        | "double" -> return typeof<double>
        | "float" -> return typeof<single>
        | "int" -> return typeof<int>
        | "uint" -> return typeof<System.UInt32>
        | "long" -> return typeof<System.Int64>
        | "ulong" -> return typeof<System.UInt64>
        | "object" -> return typeof<obj>
        | "short" -> return typeof<System.Int16>
        | "ushort" -> return typeof<System.UInt16>
        | "string" -> return typeof<string>
        | s ->
            let type' = System.Type.GetType(s, false)
            let type' = if type' <> null then type' else System.Type.GetType("System." + s, false)

            if type' <> null then
                return type'
            else
                return! Failure (sprintf "Type '%s' not found." s)
    }

    let parseVisibility visibilityText = result {
        match visibilityText with
        | "public" -> return Public
        | "internal" -> return Internal
        | s -> return! Failure (sprintf "Visibility can't be parsed: '%s'" s)
    }

    let typeDefinitionToType (content:LineContent) = result {
        match content with
        | TypeDefinition(visibility,namespace',name,underlyingType) ->
            let! underlyingType = parseUnderlyingType underlyingType
            let! visibility = parseVisibility visibility
            return makeIdType underlyingType (fun idType ->
                { idType with
                    Name = name
                    Namespace = namespace'
                    Visibility = visibility
                }
            )
        | _ -> return! Failure "Not a type definition"
    }

    let propertyWithNoValidType (line:ParsedLine) =
        {
            Line = line.Line;
            ErrorText = "Property line associated with no valid type";
            IsInvalidType = false
        }

    let rec linesToIdTypes' newCurrentType remainingLines typeToAdd errorToAdd : IdType list * ParseError list =
        let (innerTypes, innerErrors) = linesToIdTypes newCurrentType remainingLines
        let innerTypes = match typeToAdd with | Some(t) -> t :: innerTypes | _ -> innerTypes
        let innerErrors = match errorToAdd with | Some(t) -> t :: innerErrors | _ -> innerErrors
        (innerTypes, innerErrors)

    and linesToIdTypes currentType remainingLines =
        match remainingLines with
        | line :: rest ->
            // Helper functions to make to rest clearer
            let reportTypeError error = linesToIdTypes' Option.None rest currentType (Some(error))
            let reportPropertyError error = linesToIdTypes' currentType rest Option.None (Some(error))
            let finishCurrentType newType = linesToIdTypes' (Some(newType)) rest currentType Option.None
            let continueOnType type' = linesToIdTypes (Some(type')) rest

            match line with
            | Success(line) ->
                match line.Content with
                | Property(_,_) -> 
                    match currentType with
                    | Some(idType) -> 
                        let idType = idType |> addProperty line.Content
                        match idType with
                        | Success(idType) -> continueOnType idType
                        | Failure(error) -> reportPropertyError (ParseError.fromString error line.Line)
                    | Option.None -> reportPropertyError (propertyWithNoValidType line)
                | TypeDefinition(_,_,_,_) ->
                    let newType = typeDefinitionToType line.Content
                    match newType with
                    | Success(newType) -> finishCurrentType newType
                    | Failure(error) -> reportTypeError (ParseError.fromString error line.Line)
            | Failure(error) ->
                // If a type line was invalid we don't want the following properties to be associated with the previous type
                // But if a property is invalid we want the following properties to be associated with the current type
                match (error.IsInvalidType, currentType) with
                | (true, Some(currentType)) -> reportTypeError error
                | _ -> reportPropertyError error
        | [] -> 
            match currentType with
            | Some(idType) -> ([idType], [])
            | Option.None -> ([], [])

    let linesToConfiguration (lines:Result<ParsedLine,ParseError> seq) : Configuration =
        let lines = lines |> List.ofSeq
        let (types, errors) = linesToIdTypes Option.None lines
        { Path = Option.None; Types = types; Errors = errors }

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

/// Load a '.stidgen' file content from a file into a Configuration
/// data-structure
let loadFromFile (file:FileInfo) =
    use reader = new FileStream(file.FullName, FileMode.Open)
    let configuration = loadFromStream reader
    { configuration with Path = Some(file.FullName) }
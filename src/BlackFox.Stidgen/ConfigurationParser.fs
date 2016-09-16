module BlackFox.Stidgen.ConfigurationParser 
    
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.Control

type Property =
    | AllowNull of bool
    | CastToUnderlying of Cast
    | CastFromUnderlying  of Cast
    | InternString of bool
    | EqualsUnderlying of bool
    | ProtobufnetSerializable of bool
    | FileName of string option
    | ValueProperty of string

type IdTypesConfig =
    {
        Visibility: ClassVisibility
        FullName: string list
        UnderlyingType: string List
        Properties: Property list
    }

module Combinators =
    open FParsec
    open FParsecExts
    open System
    open System.Globalization

    let private pVisibility = (stringReturn "internal" ClassVisibility.Internal) <|> (stringReturn "public" ClassVisibility.Public)
    let private identifier =
        let identifierFirstLetterCats =
            [
                UnicodeCategory.LowercaseLetter
                UnicodeCategory.UppercaseLetter
                UnicodeCategory.TitlecaseLetter
                UnicodeCategory.ModifierLetter
                UnicodeCategory.OtherLetter
            ]

        let identifierRestCats =
            identifierFirstLetterCats @ [
                UnicodeCategory.LetterNumber
                UnicodeCategory.NonSpacingMark
                UnicodeCategory.SpacingCombiningMark
                UnicodeCategory.DecimalDigitNumber
                UnicodeCategory.ConnectorPunctuation
                UnicodeCategory.Format
            ]

        let inUnicodeCategories cats = (fun c -> cats |> Seq.contains (Char.GetUnicodeCategory(c)))
        let identifierFirstLetter = inUnicodeCategories identifierFirstLetterCats
        let identifierRest = inUnicodeCategories identifierRestCats

        many1Satisfy2 identifierFirstLetter identifierRest <?> "C# identifier"

    let private identifierWithNs = sepBy1 identifier (pchar '.')

    let private skipSpaceOrTab = skipManySatisfy (fun c -> c = ' ' || c = '\t')
    let private skipSpaceOrTab1 = skipMany1Satisfy (fun c -> c = ' ' || c = '\t')

    let private pComment = (pstring "#" <|> pstring "//") >>. restOfLine false

    let private pTypeDefinition =
        skipSpaceOrTab
        >>. pVisibility
        .>> skipSpaceOrTab1
        .>>. identifierWithNs
        .>> pchar '<'
        .>>. identifierWithNs
        .>> pchar '>'
        .>> skipSpaceOrTab

    let private pProperty name pValue (f: _ -> Property) = skipStringCI name >>. skipSpaceOrTab >>. skipChar ':' >>. skipSpaceOrTab >>. pValue |>> f
    let private pBoolean = stringCIReturn "true" true <|> stringCIReturn "false" false .>> skipSpaceOrTab
    let private pCast = stringCIReturn "explicit" Explicit <|> stringCIReturn "implicit" Implicit .>> skipSpaceOrTab
    let private pOptionalString = restOfLine false |>> (fun s -> if String.IsNullOrEmpty(s) then Option.None else Some(s.Trim()))
    let private pForcedString = restOfLine false |>> (fun s -> s.Trim())
    let private pProperties =
        choice [
            pProperty "AllowNull" pBoolean AllowNull
            pProperty "CastToUnderlying" pCast CastToUnderlying
            pProperty "CastFromUnderlying" pCast CastFromUnderlying
            pProperty "InternString" pBoolean InternString
            pProperty "EqualsUnderlying" pBoolean EqualsUnderlying
            pProperty "ProtobufnetSerializable" pBoolean ProtobufnetSerializable
            pProperty "FileName" pOptionalString FileName
            pProperty "ValueProperty" pForcedString ValueProperty
        ]
    let private pPropertyLine = skipSpaceOrTab >>. pProperties

    let private isNewlineOrSpace = fun c -> c = '\r' || c = '\n' || c = ' '|| c = '\t'
    let private newLines = many (many1Satisfy isNewlineOrSpace <|> pComment)
    let private newLines1 = satisfy isNewlineOrSpace >>. newLines

    let private parsedToConfig (((visibility, fullName), underlyingType), properties) =
        {
            Visibility = visibility
            FullName = fullName
            UnderlyingType = underlyingType
            Properties = match properties with |Some(props) -> props |Option.None -> []
        }

    let private pTypeLines = pTypeDefinition .>>. opt (newLines >>? sepBy1BacktrackEnd pPropertyLine newLines1) |>> parsedToConfig

    let private pFile = newLines >>. sepEndBy pTypeLines newLines1 .>> eof

    let runConfigurationParser str = run pFile str

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
        let type' = if isNull type' then  System.Type.GetType("System." + s, false) else type'

        if isNull type' then
            return! Failure (sprintf "Type '%s' not found." s)
        else
            return type'
}

let applyProperty idType = function
    | AllowNull b -> { idType with AllowNull = b }
    | CastToUnderlying c -> { idType with CastToUnderlying = c }
    | CastFromUnderlying c -> { idType with CastFromUnderlying = c }
    | InternString b -> { idType with InternString = b }
    | EqualsUnderlying b -> { idType with EqualsUnderlying = b }
    | ProtobufnetSerializable b -> { idType with ProtobufnetSerializable = b }
    | FileName s -> { idType with FileName = s }
    | ValueProperty s -> { idType with ValueProperty = s }

let idTypeConfigToIdType config =
    let underlyingType = System.String.Join (".", config.UnderlyingType)
    match parseUnderlyingType underlyingType with
    | Failure _ -> Failure config
    | Success t ->
        Success (makeIdType t (fun idType ->
            let withProperties = config.Properties |> List.fold applyProperty idType
            { withProperties with
                Name = config.FullName |> List.last
                Visibility = config.Visibility
                Namespace = System.String.Join(".", config.FullName |> List.take (config.FullName.Length-1))
            }
        ))

type ConfigurationFailures =
    | ParseError of message: string * parserError: FParsec.Error.ParserError
    | InvalidUnderlyingTypes of IdTypesConfig list

type Configuration =
    {
        /// Path of the '.stidgen' file
        Path : string option
        
        /// Configuration parsing result
        Result: Result<IdType list, ConfigurationFailures>
    }
with
    member x.HasErrors () =
        match x.Result with | Success _ -> false | Failure _ -> true
    member x.Types
        with get() =
            match x.Result with | Success types -> types | Failure e -> failwithf "No types, error: %A" e

let parseConfiguration str =
    let parseResult = Combinators.runConfigurationParser str
    let result =
        match parseResult with
        | FParsec.CharParsers.ParserResult.Success(result, _, _) ->
            let transformedResults = result |> List.map idTypeConfigToIdType
            let transformationFailures = transformedResults |> List.choose (fun r -> match r with Success _ -> Option.None|Failure x -> Some(x))
            if Seq.isEmpty transformationFailures then
                let types' = transformedResults |> List.map(fun r -> match r with Success x -> x|Failure _ -> failwith "Unexpected")
                Success(types')
            else
                Failure(InvalidUnderlyingTypes(transformationFailures))
        | FParsec.CharParsers.ParserResult.Failure(message, err, _) ->
            Failure(ParseError(message, err))

    { Path = Option.None; Result = result }

let loadFromLines (lines:string seq) =
    System.String.Join("\n", lines)
    |> parseConfiguration

open System.IO

let loadFromTextReader (reader:TextReader) = 
    reader.ReadToEnd() |> parseConfiguration

let loadFromStream (stream:Stream) =
    use reader = new StreamReader(stream)
    loadFromTextReader reader

/// Load a '.stidgen' file content from a file into a Configuration
/// data-structure
let loadFromFile (file:FileInfo) =
    use reader = new FileStream(file.FullName, FileMode.Open)
    let configuration = loadFromStream reader
    { configuration with Path = Some(file.FullName) }
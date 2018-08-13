module BlackFox.Stidgen.ConfigurationParserTests

open BlackFox.Stidgen.ConfigurationParser
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.Control
open Expecto.Tests
open Expecto

let propertyTest propName testValue (expectedValue:'a) (extractValueFromIdType : IdType -> 'a) =
    let t =
        ([
            "public SomeTypeName<int>"
            (sprintf "    %s: %s" propName testValue)
        ] |> loadFromLines).Types |> List.head
    Expect.equal (t |> extractValueFromIdType)  expectedValue ""

let expectParseError textContent conf =
    match conf.Result with
    | Failure (ConfigurationParser.ParseError(msg, _)) ->
        Expect.stringContains msg textContent ""
    | _ -> failwithf "Expected a parse error but got %A" conf.Result

let expectInvalidTypes types conf =
    match conf.Result with
    | Failure (ConfigurationParser.InvalidUnderlyingTypes(invalidTypes)) ->
        let invalidTypes = invalidTypes |> List.map (fun t -> System.String.Join(".", t.UnderlyingType)) |> List.sort
        let types = types |> List.sort

        Expect.equal (invalidTypes)  types ""

    | _ -> failwithf "Expected a parse error but got %A" conf.Result

let tests = [
    test "Simple type" {

        let types = (["public SomeName.Space.TypeName<string>"] |> loadFromLines).Types
        Expect.equal (types.Length)  1 ""
        let idType = types.Head
        Expect.equal (idType.UnderlyingType)  typeof<string> ""
        Expect.equal (idType.Name)  "TypeName" ""
        Expect.equal (idType.Namespace)  "SomeName.Space" ""
        Expect.equal (idType.Visibility)  ClassVisibility.Public ""

    }

    test "Multiple types" {

        let types =
            ([
                "public SomeName.Space.AName<int>"
                "public AGuid<Guid>"
                "public SomeName.Space.TypeName<string>"
            ] |> loadFromLines).Types |> List.toArray
        Expect.equal (types.Length)  3 ""
        Expect.equal (types.[0].Name)  "AName" ""
        Expect.equal (types.[1].Name)  "AGuid" ""
        Expect.equal (types.[2].Name)  "TypeName" ""

    }

    test "Can load text with comments in the middle" {

        let types =
            ([
                "public SomeName.Space.TypeName<string>"
                "#Comment"
                "AllowNull: true"
                "public SomeName.Space.TypeName2<string>"
            ] |> loadFromLines).Types
        Expect.equal (types.Length)  2 ""

    }

    test "Can load text with comments#" {

        let types =
            ([
                "# This is a comment"
                ""
                "public SomeName.Space.TypeName<string>"
            ] |> loadFromLines).Types
        Expect.equal (types.Length)  1 ""

    }

    test "Can load text with comments" {

        let types =
            ([
                "// This is a comment"
                ""
                "public SomeName.Space.TypeName<string>"
            ] |> loadFromLines).Types
        Expect.equal (types.Length)  1 ""

    }

    test "No text" {

        let types = ([] |> loadFromLines).Types
        Expect.equal (types.Length)  0 ""

    }

    test "Comments only" {

        let types =
            ([
                "// This is a comment"
                ""
            ] |> loadFromLines).Types
        Expect.equal (types.Length)  0 ""

    }

    test "Type without namespace" {

        let t = (["internal SomeTypeName<int>"] |> loadFromLines).Types |> List.head
        Expect.equal (t.Namespace)  ""
 ""
    }
    
    testWithParams
        "Underlying of type"
        [
            "bool", "System.Boolean"
            "byte", "System.Byte"
            "sbyte", "System.SByte"
            "char", "System.Char"
            "decimal", "System.Decimal"
            "double", "System.Double"
            "float", "System.Single"
            "int", "System.Int32"
            "uint", "System.UInt32"
            "long", "System.Int64"
            "ulong", "System.UInt64"
            "object", "System.Object"
            "short", "System.Int16"
            "ushort", "System.UInt16"
            "string", "System.String"
            "Guid", "System.Guid"
        ]
        (fun (name, expectedType) -> 
            let s = sprintf "internal SomeTypeName<%s>" name
            let t = ([s] |> loadFromLines).Types |> List.head
            Expect.equal (t.UnderlyingType)  (System.Type.GetType(expectedType)) "")

    test "Internal visibility" {
        let t = (["internal SomeTypeName<int>"] |> loadFromLines).Types |> List.head
        Expect.equal (t.Visibility)  ClassVisibility.Internal ""
    }

    test "Public visibility" {
        let t = (["public SomeTypeName<int>"] |> loadFromLines).Types |> List.head
        Expect.equal (t.Visibility)  ClassVisibility.Public ""
    }

    test "Property ValueProperty" {

        propertyTest "ValueProperty" "MyValue" "MyValue" (fun t -> t.ValueProperty)
    }

    testWithParams "Property AllowNull" ["true", true;"false",false] <| fun (text, expected) ->
        propertyTest "AllowNull" text expected (fun t -> t.AllowNull)

    testWithParams "Property InternString" ["true", true;"false",false] <| fun (text, expected) ->
        propertyTest "InternString" text expected (fun t -> t.InternString)

    testWithParams "Property EqualsUnderlying" ["true", true;"false",false] <| fun (text, expected) ->
        propertyTest "EqualsUnderlying" text expected (fun t -> t.EqualsUnderlying)

    test "Property CastToUnderlying Explicit" {
        propertyTest "CastToUnderlying" "Explicit" Explicit (fun t -> t.CastToUnderlying)
    }

    test "Property CastToUnderlying Implicit" {
        propertyTest "CastToUnderlying" "Implicit" Implicit (fun t -> t.CastToUnderlying)
    }

    test "Property CastFromUnderlying Explicit" {
        propertyTest "CastFromUnderlying" "Explicit" Explicit (fun t -> t.CastFromUnderlying)
    }

    test "Property CastFromUnderlying Implicit" {
        propertyTest "CastFromUnderlying" "Implicit" Implicit (fun t -> t.CastFromUnderlying)
    }

    test "Property FileName with value" {
        propertyTest "FileName" "My File.cs" (Some("My File.cs")) (fun t -> t.FileName)
    }

    test "Property FileName without value" {
        propertyTest "FileName" "" Option.None (fun t -> t.FileName)
    }

    test "Invalid type no space" {

        let conf =
            ([
                "HelloWorld"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 1 Col: 1"

    }

    test "Invalid type too much space" {

        let conf =
            ([
                "Hello World 42"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 1 Col: 1"

    }

    test "Invalid type and valid types" {

        let conf =
            ([
                "public SomeName.Space.AName<int>"
                "HelloWorld"
                "public SomeName.Space.TypeName<string>"
                "public SomeName.Space.OtherTypeName<string>"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 2 Col: 1"

    }

    test "Invalid type no underlying" {

        let conf =
            ([
                "public HelloWorld"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 1 Col: 18"

    }

    test "Invalid type no underlying end" {

        let conf =
            ([
                "public HelloWorld<int"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 1 Col: 22"

    }

    test "Invalid type invalid underlying type" {

        let conf =
            ([
                "public HelloWorld<System.DoNotExists>"
            ] |> loadFromLines)
        conf |> expectInvalidTypes ["System.DoNotExists"]

    }

    test "Property with invalid delimiter" {

        let conf =
            ([
                "public TestId<int>"
                "    Test=Value"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 2 Col: 5"

    }

    test "Property without type" {

        let conf =
            ([
                "    AllowNull:true"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 1 Col: 5"

    }

    test "Property with invalid type" {

        let conf =
            ([
                "42"
                "    AllowNull:true"
            ] |> loadFromLines)
        conf |> expectParseError "Error in Ln: 1 Col: 1"
    }
]

[<Tests>]
let test = testList "Equality" tests

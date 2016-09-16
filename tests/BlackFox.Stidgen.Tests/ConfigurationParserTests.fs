module BlackFox.Stidgen.ConfigurationParserTests

open BlackFox.Stidgen.ConfigurationParser
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.Control
open FsUnit
open NUnit.Framework

[<Test>]
let ``Simple type`` () = 
    let types = (["public SomeName.Space.TypeName<string>"] |> loadFromLines).Types
    types.Length |> should equal 1
    let idType = types.Head
    idType.UnderlyingType |> should equal typeof<string>
    idType.Name |> should equal "TypeName"
    idType.Namespace |> should equal "SomeName.Space"
    idType.Visibility |> should equal ClassVisibility.Public

[<Test>]
let ``Multiple types`` () = 
    let types =
        ([
            "public SomeName.Space.AName<int>"
            "public AGuid<Guid>"
            "public SomeName.Space.TypeName<string>"
        ] |> loadFromLines).Types |> List.toArray
    types.Length |> should equal 3
    types.[0].Name |> should equal "AName"
    types.[1].Name |> should equal "AGuid"
    types.[2].Name |> should equal "TypeName"

[<Test>]
let ``Can load text with comments in the middle`` () = 
    let types =
        ([
            "public SomeName.Space.TypeName<string>"
            "#Comment"
            "AllowNull: true"
            "public SomeName.Space.TypeName2<string>"
        ] |> loadFromLines).Types
    types.Length |> should equal 2

[<Test>]
let ``Can load text with comments#`` () = 
    let types =
        ([
            "# This is a comment"
            ""
            "public SomeName.Space.TypeName<string>"
        ] |> loadFromLines).Types
    types.Length |> should equal 1

[<Test>]
let ``Can load text with comments`` () = 
    let types =
        ([
            "// This is a comment"
            ""
            "public SomeName.Space.TypeName<string>"
        ] |> loadFromLines).Types
    types.Length |> should equal 1

[<Test>]
let ``No text`` () = 
    let types = ([] |> loadFromLines).Types
    types.Length |> should equal 0

[<Test>]
let ``Comments only`` () = 
    let types =
        ([
            "// This is a comment"
            ""
        ] |> loadFromLines).Types
    types.Length |> should equal 0

[<Test>]
let ``Type without namespace`` () = 
    let t = (["internal SomeTypeName<int>"] |> loadFromLines).Types |> List.head
    t.Namespace |> should equal ""
    
[<TestCase("bool", "System.Boolean")>]
[<TestCase("byte", "System.Byte")>]
[<TestCase("sbyte", "System.SByte")>]
[<TestCase("char", "System.Char")>]
[<TestCase("decimal", "System.Decimal")>]
[<TestCase("double", "System.Double")>]
[<TestCase("float", "System.Single")>]
[<TestCase("int", "System.Int32")>]
[<TestCase("uint", "System.UInt32")>]
[<TestCase("long", "System.Int64")>]
[<TestCase("ulong", "System.UInt64")>]
[<TestCase("object", "System.Object")>]
[<TestCase("short", "System.Int16")>]
[<TestCase("ushort", "System.UInt16")>]
[<TestCase("string", "System.String")>]
[<TestCase("System.Collections.ArrayList", "System.Collections.ArrayList")>]
[<TestCase("Guid", "System.Guid")>]
let ``Underlying of type`` name expectedType =
    let s = sprintf "internal SomeTypeName<%s>" name
    let t = ([s] |> loadFromLines).Types |> List.head
    t.UnderlyingType |> should equal (System.Type.GetType(expectedType))

[<Test>]
let ``Internal visibility`` () = 
    let t = (["internal SomeTypeName<int>"] |> loadFromLines).Types |> List.head
    t.Visibility |> should equal ClassVisibility.Internal

[<Test>]
let ``Public visibility`` () = 
    let t = (["public SomeTypeName<int>"] |> loadFromLines).Types |> List.head
    t.Visibility |> should equal ClassVisibility.Public

let propertyTest propName testValue (expectedValue:'a) (extractValueFromIdType : IdType -> 'a) =
    let t =
        ([
            "public SomeTypeName<int>"
            (sprintf "    %s: %s" propName testValue)
        ] |> loadFromLines).Types |> List.head
    t |> extractValueFromIdType |> should equal expectedValue

[<Test>]
let ``Property ValueProperty`` () =
    propertyTest "ValueProperty" "MyValue" "MyValue" (fun t -> t.ValueProperty)

[<TestCase("true", true)>]
[<TestCase("false", false)>]
let ``Property AllowNull`` text (expected:bool) =
    propertyTest "AllowNull" text expected (fun t -> t.AllowNull)

[<TestCase("true", true)>]
[<TestCase("false", false)>]
let ``Property InternString`` text (expected:bool) =
    propertyTest "InternString" text expected (fun t -> t.InternString)

[<TestCase("true", true)>]
[<TestCase("false", false)>]
let ``Property EqualsUnderlying`` text (expected:bool) =
    propertyTest "EqualsUnderlying" text expected (fun t -> t.EqualsUnderlying)

[<Test>]
let ``Property CastToUnderlying Explicit`` () =
    propertyTest "CastToUnderlying" "Explicit" Explicit (fun t -> t.CastToUnderlying)

[<Test>]
let ``Property CastToUnderlying Implicit`` () =
    propertyTest "CastToUnderlying" "Implicit" Implicit (fun t -> t.CastToUnderlying)

[<Test>]
let ``Property CastFromUnderlying Explicit`` () =
    propertyTest "CastFromUnderlying" "Explicit" Explicit (fun t -> t.CastFromUnderlying)

[<Test>]
let ``Property CastFromUnderlying Implicit`` () =
    propertyTest "CastFromUnderlying" "Implicit" Implicit (fun t -> t.CastFromUnderlying)

[<Test>]
let ``Property FileName with value`` () =
    propertyTest "FileName" "My File.cs" (Some("My File.cs")) (fun t -> t.FileName)

[<Test>]
let ``Property FileName without value`` () =
    propertyTest "FileName" "" Option.None (fun t -> t.FileName)

let expectParseError textContent conf =
    match conf.Result with
    | Failure (ConfigurationParser.ParseError(msg, _)) ->
        msg |> should contain textContent
    | _ -> failwithf "Expected a parse error but got %A" conf.Result

let expectInvalidTypes types conf =
    match conf.Result with
    | Failure (ConfigurationParser.InvalidUnderlyingTypes(invalidTypes)) ->
        let invalidTypes = invalidTypes |> List.map (fun t -> System.String.Join(".", t.UnderlyingType)) |> List.sort
        let types = types |> List.sort

        invalidTypes |> should equal types

    | _ -> failwithf "Expected a parse error but got %A" conf.Result

[<Test>]
let ``Invalid type no space`` () =
    let conf =
        ([
            "HelloWorld"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 1 Col: 1"

[<Test>]
let ``Invalid type too much space`` () =
    let conf =
        ([
            "Hello World 42"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 1 Col: 1"

[<Test>]
let ``Invalid type and valid types`` () =
    let conf =
        ([
            "public SomeName.Space.AName<int>"
            "HelloWorld"
            "public SomeName.Space.TypeName<string>"
            "public SomeName.Space.OtherTypeName<string>"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 2 Col: 1"

[<Test>]
let ``Invalid type no underlying`` () =
    let conf =
        ([
            "public HelloWorld"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 1 Col: 18"

[<Test>]
let ``Invalid type no underlying end`` () =
    let conf =
        ([
            "public HelloWorld<int"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 1 Col: 22"

[<Test>]
let ``Invalid type invalid underlying type`` () =
    let conf =
        ([
            "public HelloWorld<System.DoNotExists>"
        ] |> loadFromLines)
    conf |> expectInvalidTypes ["System.DoNotExists"]

[<Test>]
let ``Property with invalid delimiter`` () =
    let conf =
        ([
            "public TestId<int>"
            "    Test=Value"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 2 Col: 5"

[<Test>]
let ``Property without type`` () =
    let conf =
        ([
            "    AllowNull:true"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 1 Col: 5"

[<Test>]
let ``Property with invalid type`` () =
    let conf =
        ([
            "42"
            "    AllowNull:true"
        ] |> loadFromLines)
    conf |> expectParseError "Error in Ln: 1 Col: 1"
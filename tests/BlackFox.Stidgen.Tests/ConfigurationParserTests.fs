module BlackFox.Stidgen.ConfigurationParserTests

open BlackFox.Stidgen.ConfigurationParser
open BlackFox.Stidgen.Description
open FsUnit
open NUnit.Framework

[<Test>]
let ``Simple type`` () = 
    let types = ["public SomeName.Space.TypeName<string>"] |> loadFromLines
    types.Length |> should equal 1
    let idType = types.Head
    idType.UnderlyingType |> should equal typeof<string>
    idType.Name |> should equal "TypeName"
    idType.Namespace |> should equal "SomeName.Space"
    idType.Visibility |> should equal ClassVisibility.Public

[<Test>]
let ``Multiple types`` () = 
    let types =
        [
            "public SomeName.Space.AName<int>"
            "public AGuid<Guid>"
            "public SomeName.Space.TypeName<string>"
        ] |> loadFromLines |> List.toArray
    types.Length |> should equal 3
    types.[0].Name |> should equal "AName"
    types.[1].Name |> should equal "AGuid"
    types.[2].Name |> should equal "TypeName"

[<Test>]
let ``Can load text with comments`` () = 
    let types =
        [
            "// This is a comment"
            ""
            "public SomeName.Space.TypeName<string>"
        ] |> loadFromLines
    types.Length |> should equal 1

[<Test>]
let ``No text`` () = 
    let types = [] |> loadFromLines
    types.Length |> should equal 0

[<Test>]
let ``Comments only`` () = 
    let types =
        [
            "// This is a comment"
            ""
        ] |> loadFromLines
    types.Length |> should equal 0

[<Test>]
let ``Type without namespace`` () = 
    let t = ["internal SomeTypeName<int>"] |> loadFromLines |> List.head
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
    let t = [s] |> loadFromLines |> List.head
    t.UnderlyingType |> should equal (System.Type.GetType(expectedType))

[<Test>]
let ``Internal visibility`` () = 
    let t = ["internal SomeTypeName<int>"] |> loadFromLines |> List.head
    t.Visibility |> should equal ClassVisibility.Internal

[<Test>]
let ``Public visibility`` () = 
    let t = ["public SomeTypeName<int>"] |> loadFromLines |> List.head
    t.Visibility |> should equal ClassVisibility.Public

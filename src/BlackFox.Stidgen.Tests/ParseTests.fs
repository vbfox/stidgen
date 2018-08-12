module BlackFox.Stidgen.ParseTests

open BlackFox.Stidgen.CsharpCodeTesting
open BlackFox.Stidgen.Description
open System
open Expecto.Tests
open System.Globalization
open System.Threading
open Expecto

let withCulture (culture: string) f () =    
    let previous = Thread.CurrentThread.CurrentCulture
    try
        Thread.CurrentThread.CurrentCulture <- CultureInfo.GetCultureInfo(culture)
        f ()
    finally
        Thread.CurrentThread.CurrentCulture <- previous

let testWithCulture name culture f =
    testCase name (withCulture culture f)

type IParseTests =
    abstract member testCorrectTryParse<'t> : string -> string -> string -> unit
    abstract member testInvalidTryParse<'t> : string -> string -> unit

let standardTests (x: IParseTests) = [
    testWithCulture "good Int32" "en-US" <| fun () ->
        x.testCorrectTryParse<int> "42" "" "IsEqualTo(42)"

    test "good Int32 hex" {
        x.testCorrectTryParse<int> "2A" ", NumberStyles.HexNumber, CultureInfo.InvariantCulture" "IsEqualTo(42)"
    }

    testWithCulture "bad Int32" "en-US" <| fun () ->
        x.testInvalidTryParse<int> "foo" ""

    testWithCulture "good Double" "en-US" <| fun () ->
        x.testCorrectTryParse<float> "3.14159" "" "IsEqualTo(3.14159)"

    testWithCulture "good Double fr-FR" "fr-FR" <| fun () ->
        x.testCorrectTryParse<float> "3,14159" "" "IsEqualTo(3.14159)"

    test "good Double fr-FR with NumberStyles" {
        x.testCorrectTryParse<float>
            "3,14159"
            @", NumberStyles.Any, CultureInfo.GetCultureInfo(""fr-FR"")"
            "IsEqualTo(3.14159)"
    }

    testWithCulture "bad Double" "en-US" <| fun () ->
        x.testInvalidTryParse<float> "foo" ""

    test "good Guid" {
         x.testCorrectTryParse<Guid>
            "{0050AC52-49E9-11E5-95FD-03788BD11E5D}"
            ""
            @"IsEqualTo(new Guid(""{0050AC52-49E9-11E5-95FD-03788BD11E5D}""))"
    }

    test "bad Guid" {
        x.testInvalidTryParse<Guid> "foo" ""
    }
]

let tests = [
    testList "TryParse" (standardTests {
        new IParseTests with
            member __.testCorrectTryParse<'t> input additionalArgs additionalTest =
                let idType = makeIdFromType<'t> id
                let test = sprintf @"
                    Id instance;
                    var result = Id.TryParse(""%s""%s, out instance);
                    Check.That(result).IsTrue();
                    Check.That(instance.Value).%s;
                    "
                            input additionalArgs additionalTest

                runGeneratedTest idType test

            member __.testInvalidTryParse<'t> input additionalArgs =
                let idType = makeIdFromType<'t> id
                let test = sprintf @"
                    Id instance;
                    var result = Id.TryParse(""%s""%s, out instance);
                    Check.That(result).IsFalse();
                    "
                            input additionalArgs

                runGeneratedTest idType test
    })

    testList "TryParse nullable" (standardTests {
        new IParseTests with
            member __.testCorrectTryParse<'t> input additionalArgs additionalTest =
                let idType = makeIdFromType<'t> id
    
                let test = sprintf @"
                    var result = Id.TryParse(""%s""%s);
                    Check.That(result).IsNotNull();
                    Check.That(result.Value.Value).%s;
                    "
                            input additionalArgs additionalTest

                runGeneratedTest idType test

            member __.testInvalidTryParse<'t> input additionalArgs =
                let idType = makeIdFromType<'t> id
                let test = sprintf @"
                    var result = Id.TryParse(""%s""%s);
                    Check.That(result).IsNull();
                    "
                            input additionalArgs

                runGeneratedTest idType test
    })

    testList "Parse" (standardTests {
        new IParseTests with
            member __.testCorrectTryParse<'t> input additionalArgs additionalTest =
                let idType = makeIdFromType<'t> id
    
                let test = sprintf @"
                    Check.ThatCode(() => Id.Parse(""%s""%s)).DoesNotThrow();
                    var result = Id.Parse(""%s""%s);
                    Check.That(result.Value).%s;
                    "
                            input additionalArgs input additionalArgs additionalTest

                runGeneratedTest idType test

            member __.testInvalidTryParse<'t> input additionalArgs =
                let idType = makeIdFromType<'t> id
                let test = sprintf @"
                    Check.ThatCode(() => Id.Parse(""%s""%s)).Throws<FormatException>();
                    "
                            input additionalArgs

                runGeneratedTest idType test
    })
]

[<Tests>]
let test = testList "Parse" tests
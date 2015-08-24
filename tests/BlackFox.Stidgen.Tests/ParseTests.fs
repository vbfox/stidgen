module BlackFox.Stidgen.ParseTests

open BlackFox.Stidgen.CsharpCodeTesting
open BlackFox.Stidgen.CsharpGeneration
open BlackFox.Stidgen.Description
open NUnit.Framework
open System

[<AbstractClass>]
type ParsingTests() =
    abstract member testCorrectTryParse<'t> : string -> string -> string -> unit
    abstract member testInvalidTryParse<'t> : string -> string -> unit

    [<Test>] [<SetCulture("en-US")>]
    member x.``good Int32`` () = x.testCorrectTryParse<int> "42" "" "IsEqualTo(42)"

    [<Test>]
    member x.``good Int32 hex`` () =
        x.testCorrectTryParse<int> "2A" ", NumberStyles.HexNumber, CultureInfo.InvariantCulture" "IsEqualTo(42)"

    [<Test>] [<SetCulture("en-US")>]
    member x.``bad Int32`` () = x.testInvalidTryParse<int> "foo" ""

    [<Test>] [<SetCulture("en-US")>]
    member x.``good Double`` () = x.testCorrectTryParse<float> "3.14159" "" "IsEqualTo(3.14159)"

    [<Test>] [<SetCulture("fr-FR")>]
    member x.``good Double fr-FR`` () = x.testCorrectTryParse<float> "3,14159" "" "IsEqualTo(3.14159)"

    [<Test>]
    member x.``good Double fr-FR with NumberStyles`` () =
        x.testCorrectTryParse<float>
            "3,14159"
            @", NumberStyles.Any, CultureInfo.GetCultureInfo(""fr-FR"")"
            "IsEqualTo(3.14159)"

    [<Test>] [<SetCulture("en-US")>]
    member x.``bad Double`` () = x.testInvalidTryParse<float> "foo" ""

    [<Test>]
    member x.``good Guid`` () =
         x.testCorrectTryParse<Guid>
            "{0050AC52-49E9-11E5-95FD-03788BD11E5D}"
            ""
            @"IsEqualTo(new Guid(""{0050AC52-49E9-11E5-95FD-03788BD11E5D}""))"

    [<Test>]
    member x.``bad Guid`` () =  x.testInvalidTryParse<Guid> "foo" ""

[<TestFixture>]
type TryParse() =
    inherit ParsingTests()

    override x.testCorrectTryParse<'t> input additionalArgs additionalTest =
        let idType = makeIdFromType<'t> id
        let test = sprintf @"
            Id instance;
            var result = Id.TryParse(""%s""%s, out instance);
            Check.That(result).IsTrue();
            Check.That(instance.Value).%s;
            "
                    input additionalArgs additionalTest

        runGeneratedTest idType test

    override x.testInvalidTryParse<'t> input additionalArgs =
        let idType = makeIdFromType<'t> id
        let test = sprintf @"
            Id instance;
            var result = Id.TryParse(""%s""%s, out instance);
            Check.That(result).IsFalse();
            "
                    input additionalArgs

        runGeneratedTest idType test

[<TestFixture>]
type ``TryParse nullable``() =
    inherit ParsingTests()

    override x.testCorrectTryParse<'t> input additionalArgs additionalTest =
        let idType = makeIdFromType<'t> id
    
        let test = sprintf @"
            var result = Id.TryParse(""%s""%s);
            Check.That(result).IsNotNull();
            Check.That(result.Value.Value).%s;
            "
                    input additionalArgs additionalTest

        runGeneratedTest idType test

    override x.testInvalidTryParse<'t> input additionalArgs =
        let idType = makeIdFromType<'t> id
        let test = sprintf @"
            var result = Id.TryParse(""%s""%s);
            Check.That(result).IsNull();
            "
                    input additionalArgs

        runGeneratedTest idType test

[<TestFixture>]
type Parse() =
    inherit ParsingTests()

    override x.testCorrectTryParse<'t> input additionalArgs additionalTest =
        let idType = makeIdFromType<'t> id
    
        let test = sprintf @"
            Check.ThatCode(() => Id.Parse(""%s""%s)).DoesNotThrow();
            var result = Id.Parse(""%s""%s);
            Check.That(result.Value).%s;
            "
                    input additionalArgs input additionalArgs additionalTest

        runGeneratedTest idType test

    override x.testInvalidTryParse<'t> input additionalArgs =
        let idType = makeIdFromType<'t> id
        let test = sprintf @"
            Check.ThatCode(() => Id.Parse(""%s""%s)).Throws<FormatException>();
            "
                    input additionalArgs

        runGeneratedTest idType test
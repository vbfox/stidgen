module BlackFox.Stidgen.CsharpGenerationTests

open BlackFox.Stidgen.CsharpCodeTesting
open BlackFox.Stidgen.CsharpGeneration
open BlackFox.Stidgen.Description
open NFluent
open NUnit.Framework
open System

[<Test>]
let ``Compile for Int32`` () = runGeneratedTest (makeIdFromType<int> id) ""

[<Test>]
let ``Compile for Nullable Int32`` () = runGeneratedTest (makeIdFromType<Nullable<int>> id) ""

[<Test>]
let ``Compile for Int64`` () = runGeneratedTest (makeIdFromType<int64> id) ""

[<Test>]
let ``Compile for String`` () = runGeneratedTest (makeIdFromType<string> id) ""

[<Test>]
let ``Compile for Guid`` () = runGeneratedTest (makeIdFromType<Guid> id) ""

[<Test>]
let ``Compile for Exception`` () = runGeneratedTest (makeIdFromType<Exception> id) ""

[<Test>]
let ``Default property name`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    var instance = new Id(""test"");
    Check.That(instance.Value).IsEqualTo(""test"");
    "

[<Test>]
let ``Custom property name`` () =
    let idType = makeIdFromType<string> (fun i ->
            { i with ValueProperty = "SomeName" }
        )

    runGeneratedTest idType @"
    var instance = new Id(""test"");
    Check.That(instance.SomeName).IsEqualTo(""test"");
    "

[<Test>]
let ``Custom type name`` () =
    let idType = makeIdFromType<string> (fun i ->
            { i with Name = "SomeName" }
        )

    runGeneratedTest idType @"
    var instance = new SomeName(""test"");
    Check.That(instance.Value).IsEqualTo(""test"");
    "

[<Test>]
let ``Custom simple namespace`` () =
    let idType = makeIdFromType<string> (fun i ->
            { i with Namespace = "SomeNs" }
        )

    runGeneratedTest idType @"
    var instance = new SomeNs.Id(""test"");
    Check.That(instance.Value).IsEqualTo(""test"");
    "

[<Test>]
let ``Custom complex namespace`` () =
    let idType = makeIdFromType<string> (fun i ->
            { i with Namespace = "Some.Ns.For.Tests" }
        )

    runGeneratedTest idType @"
    var instance = new Some.Ns.For.Tests.Id(""test"");
    Check.That(instance.Value).IsEqualTo(""test"");
    "

[<Test>]
let ``Null string throw`` () =
    let idType = makeIdFromType<string> id

    Check.ThatCode(fun () -> runGeneratedTest idType @"new Id(null);")
        .Throws<ArgumentNullException>() |> ignore

[<Test>]
let ``Null string can be allowed`` () =
    let idType = makeIdFromType<string> (fun i ->
            { i with AllowNull = true }
        )

    let test () =
        runGeneratedTest idType @"
        var instance = new Id(null);
        Check.That(instance.Value).IsEqualTo(null);
        "

    Check.ThatCode(test).DoesNotThrow() |> ignore

[<Test>]
let ``GetHashCode is lifted`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    Check.That(new Id(""Test"").Value.GetHashCode()).IsEqualTo(""Test"".GetHashCode());
    Check.That(new Id(""42"").Value.GetHashCode()).IsEqualTo(""42"".GetHashCode());
    "

[<Test>]
let ``GetHashCode works with null`` () =
    let idType = makeIdFromType<string> (fun i -> { i with AllowNull = true })

    let test () =
        runGeneratedTest idType @"
        var instance = new Id(null);
        GC.KeepAlive(instance.GetHashCode());
        "

    Check.ThatCode(test).DoesNotThrow() |> ignore

[<Test>]
let ``Value is interned`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    var instance = new Id(""Some_string_"");
    Check.That(string.IsInterned(instance.Value)).IsNotNull();
    "

[<Test>]
let ``ToString return the same string`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    var str = string.Intern(""Test"");
    var instance = new Id(str);
    Check.That(instance.ToString()).IsSameReferenceThan(str);
    "

[<Test>]
let ``Null ToString is empty string`` () =
    let idType = makeIdFromType<Nullable<int>> id
    
    runGeneratedTest idType @"
    var instance = new Id(null);
    Check.That(instance.ToString()).IsEqualTo("""");
    "

[<Test>]
let ``ToString is lifted`` () =
    let idType = makeIdFromType<Guid> id

    runGeneratedTest idType @"
    var guid = Guid.NewGuid();
    var instance = new Id(guid);
    Check.That(instance.ToString()).IsEqualTo(guid.ToString());
    "

let testCorrectTryParse<'t> input additionalTest =
    let idType = makeIdFromType<'t> id
    let test = sprintf @"
        Id instance;
        var result = Id.TryParse(""%s"", out instance);
        Check.That(result).IsTrue();
        Check.That(instance.Value).%s;
        "
                input additionalTest

    runGeneratedTest idType test

let testInvalidTryParse<'t> input =
    let idType = makeIdFromType<'t> id
    let test = sprintf @"
        Id instance;
        var result = Id.TryParse(""%s"", out instance);
        Check.That(result).IsFalse();
        "
                input

    runGeneratedTest idType test


[<Test>] [<SetCulture("en-US")>]
let ``TryParse good Int32`` () = testCorrectTryParse<int> "42" "IsEqualTo(42)"

[<Test>] [<SetCulture("en-US")>]
let ``TryParse bad Int32`` () = testInvalidTryParse<int> "foo"

[<Test>] [<SetCulture("en-US")>]
let ``TryParse good Double`` () = testCorrectTryParse<float> "3.14159" "IsEqualTo(3.14159)"

[<Test>] [<SetCulture("en-US")>]
let ``TryParse bad Double`` () = testInvalidTryParse<float> "foo"

[<Test>]
let ``TryParse good Guid`` () =
    testCorrectTryParse<Guid>
        "{0050AC52-49E9-11E5-95FD-03788BD11E5D}"
        @"IsEqualTo(new Guid(""{0050AC52-49E9-11E5-95FD-03788BD11E5D}""))"

[<Test>]
let ``TryParse bad Guid`` () = testInvalidTryParse<Guid> "foo"
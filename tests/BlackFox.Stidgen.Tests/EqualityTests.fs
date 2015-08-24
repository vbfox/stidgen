module BlackFox.Stidgen.EqualityTests

open BlackFox.Stidgen.CsharpCodeTesting
open BlackFox.Stidgen.CsharpGeneration
open BlackFox.Stidgen.Description
open FsUnit
open NUnit.Framework
open System

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

    runGeneratedTest idType @"
    var instance = new Id(null);
    Check.ThatCode(() => instance.GetHashCode()).DoesNotThrow();
    "
[<Test>]
let ``IEquatable<'t>.Equals underlying when enabled`` () =
    let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = true })

    runGeneratedTest idType @"
    IEquatable<string> instance = new Id(""Foo"");
    Check.That(instance.Equals(""Foo"")).IsTrue();
    Check.That(instance.Equals(""Bar"")).IsFalse();
    "
[<Test>]
let ``object.Equals underlying when enabled`` () =
    let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = true })

    runGeneratedTest idType @"
    object instance = new Id(""Foo"");
    Check.That(instance.Equals(""Foo"")).IsTrue();
    Check.That(instance.Equals(""Bar"")).IsFalse();
    "
[<Test>]
let ``Doesn't object.Equals underlying when disabled`` () =
    let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = false })

    runGeneratedTest idType @"
    object instance = new Id(""Foo"");
    Check.That(instance.Equals(""Foo"")).IsFalse();
    "

[<Test>]
let ``Doesn't implement IEquatable<underlying>.Equals when disabled`` () =
    let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = false })
    
    runGeneratedTest idType @"
    var instance = new Id(""Foo"");
    Check.That(instance).IsNotInstanceOf<IEquatable<string>>();
    "

[<Test>]
let ``IEquatable<'t>.Equals id`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    IEquatable<Id> instance = new Id(""Foo"");
    Check.That(instance.Equals(new Id(""Foo""))).IsTrue();
    Check.That(instance.Equals(new Id(""Bar""))).IsFalse();
    "

[<Test>]
let ``object.Equals id`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    object instance = new Id(""Foo"");
    Check.That(instance.Equals(new Id(""Foo""))).IsTrue();
    Check.That(instance.Equals(new Id(""Bar""))).IsFalse();
    "

[<Test>]
let ``== id and != id`` () =
    let idType = makeIdFromType<string> id

    runGeneratedTest idType @"
    var instance = new Id(""Foo"");
    Check.That(instance == new Id(""Foo"")).IsTrue();
    Check.That(instance == new Id(""Bar"")).IsFalse();
    Check.That(instance != new Id(""Foo"")).IsFalse();
    Check.That(instance != new Id(""Bar"")).IsTrue();
    "

[<Test>]
let ``== underlying and != underlying when enabled`` () =
    let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = true })

    runGeneratedTest idType @"
    var instance = new Id(""Foo"");
    Check.That(instance == ""Foo"").IsTrue();
    Check.That(instance == ""Bar"").IsFalse();
    Check.That(instance != ""Foo"").IsFalse();
    Check.That(instance != ""Bar"").IsTrue();
    "

[<Test>]
let ``== underlying and != underlying when disabled`` () =
    let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = false })

    let f op =
        let code = sprintf @"
        var instance = new Id(""Foo"");
        var useless = instance %s ""Foo"";
        "           op
        runGeneratedTest idType code

    let eq = fun () -> f "=="
    let neq = fun () -> f "!="

    eq |> should throw typeof<CompilationFailedException>
    neq |> should throw typeof<CompilationFailedException>
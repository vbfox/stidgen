module BlackFox.Stidgen.EqualityTests

open BlackFox.Stidgen.CsharpCodeTesting
open BlackFox.Stidgen.Description
open Expecto

let tests = [
    test "GetHashCode is lifted" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        Check.That(new Id(""Test"").Value.GetHashCode()).IsEqualTo(""Test"".GetHashCode());
        Check.That(new Id(""42"").Value.GetHashCode()).IsEqualTo(""42"".GetHashCode());
        "
    }

    test "GetHashCode works with null" {
        let idType = makeIdFromType<string> (fun i -> { i with AllowNull = true })

        runGeneratedTest idType @"
        var instance = new Id(null);
        Check.ThatCode(() => instance.GetHashCode()).DoesNotThrow();
        "
    }

    test "IComparable is implemented" {
        let idType = makeIdFromType<int> id

        runGeneratedTest idType @"
        IComparable<Id> fortyTwo = new Id(42);
        Check.That(fortyTwo.CompareTo(new Id(0))).IsEqualTo(1);
        Check.That(fortyTwo.CompareTo(new Id(42))).IsEqualTo(0);
        Check.That(fortyTwo.CompareTo(new Id(100))).IsEqualTo(-1);
        "
    }

    test "IComparable handle null" {
        let idType = makeIdFromType<string> (fun i -> { i with AllowNull = true })

        runGeneratedTest idType @"
        IComparable<Id> fortyTwo = new Id(""42"");
        IComparable<Id> nullId = new Id(null);
        Check.That(nullId.CompareTo(new Id(null))).IsEqualTo(0);
        Check.That(nullId.CompareTo(new Id(""42""))).IsEqualTo(-1);
        Check.That(fortyTwo.CompareTo(new Id(null))).IsEqualTo(1);
        "
    }
    test "IComparable<'t> compare to underlying when enabled" {
        let idType = makeIdFromType<int> (fun i -> { i with EqualsUnderlying = true })

        runGeneratedTest idType @"
        IComparable<int> instance = new Id(42);
        Check.That(instance.CompareTo(0)).IsEqualTo(1);
        Check.That(instance.CompareTo(42)).IsEqualTo(0);
        Check.That(instance.CompareTo(100)).IsEqualTo(-1);
        "
    }

    test "IComparable<Underlying> handle null" {
        let idType = makeIdFromType<string> (fun i -> { i with AllowNull = true; EqualsUnderlying = true })

        runGeneratedTest idType @"
        IComparable<string> fortyTwo = new Id(""42"");
        IComparable<string> nullId = new Id(null);
        Check.That(nullId.CompareTo(null)).IsEqualTo(0);
        Check.That(nullId.CompareTo(""42"")).IsEqualTo(-1);
        Check.That(fortyTwo.CompareTo(null)).IsEqualTo(1);
        "
    }

    test "IEquatable<'t>.Equals underlying when enabled" {
        let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = true })

        runGeneratedTest idType @"
        IEquatable<string> instance = new Id(""Foo"");
        Check.That(instance.Equals(""Foo"")).IsTrue();
        Check.That(instance.Equals(""Bar"")).IsFalse();
        "
    }

    test "object.Equals underlying when enabled" {
        let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = true })

        runGeneratedTest idType @"
        object instance = new Id(""Foo"");
        Check.That(instance.Equals(""Foo"")).IsTrue();
        Check.That(instance.Equals(""Bar"")).IsFalse();
        "
    }

    test "Doesn't object.Equals underlying when disabled" {
        let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = false })

        runGeneratedTest idType @"
        object instance = new Id(""Foo"");
        Check.That(instance.Equals(""Foo"")).IsFalse();
        "
    }

    test "Doesn't implement IEquatable<underlying>.Equals when disabled" {
        let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = false })
    
        runGeneratedTest idType @"
        var instance = new Id(""Foo"");
        Check.That(instance).IsNotInstanceOf<IEquatable<string>>();
        "
    }

    test "IEquatable<'t>.Equals id" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        IEquatable<Id> instance = new Id(""Foo"");
        Check.That(instance.Equals(new Id(""Foo""))).IsTrue();
        Check.That(instance.Equals(new Id(""Bar""))).IsFalse();
        "
    }

    test "object.Equals id" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        object instance = new Id(""Foo"");
        Check.That(instance.Equals(new Id(""Foo""))).IsTrue();
        Check.That(instance.Equals(new Id(""Bar""))).IsFalse();
        "
    }

    test "== id and != id" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        var instance = new Id(""Foo"");
        Check.That(instance == new Id(""Foo"")).IsTrue();
        Check.That(instance == new Id(""Bar"")).IsFalse();
        Check.That(instance != new Id(""Foo"")).IsFalse();
        Check.That(instance != new Id(""Bar"")).IsTrue();
        "
    }

    test "== id and != id for value type" {
        let idType = makeIdFromType<int> id

        runGeneratedTest idType @"
        var instance = new Id(42);
        Check.That(instance == new Id(42)).IsTrue();
        Check.That(instance == new Id(666)).IsFalse();
        Check.That(instance != new Id(42)).IsFalse();
        Check.That(instance != new Id(666)).IsTrue();
        "
    }

    test "== underlying and != underlying when enabled" {
        let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = true })

        runGeneratedTest idType @"
        var instance = new Id(""Foo"");
        Check.That(instance == ""Foo"").IsTrue();
        Check.That(instance == ""Bar"").IsFalse();
        Check.That(instance != ""Foo"").IsFalse();
        Check.That(instance != ""Bar"").IsTrue();
        "
    }

    test "== underlying and != underlying when enabled for value type" {
        let idType = makeIdFromType<int> (fun i -> { i with EqualsUnderlying = true })

        runGeneratedTest idType @"
        var instance = new Id(42);
        Check.That(instance == 42).IsTrue();
        Check.That(instance == 666).IsFalse();
        Check.That(instance != 42).IsFalse();
        Check.That(instance != 666).IsTrue();
        "
    }

    test "== underlying and != underlying when disabled" {
        let idType = makeIdFromType<string> (fun i -> { i with EqualsUnderlying = false })

        let f op =
            let code = sprintf @"
            var instance = new Id(""Foo"");
            var useless = instance %s ""Foo"";
            "           op
            runGeneratedTest idType code

        let eq = fun () -> f "=="
        let neq = fun () -> f "!="

        Expect.throwsT<CompilationFailedException> eq "== throw when disabled"
        Expect.throwsT<CompilationFailedException> neq "!= throw when disabled"
    }
]

[<Tests>]
let test = testList "Equality" tests
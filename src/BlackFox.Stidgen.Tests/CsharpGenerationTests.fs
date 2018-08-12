module BlackFox.Stidgen.CsharpGenerationTests

open BlackFox.Stidgen.CsharpCodeTesting
open BlackFox.Stidgen.Description
open System
open Expecto

let tests = [
    test "Compile for Int32" {
        runGeneratedTest (makeIdFromType<int> id) ""
    }

    test "Compile for Nullable Int32" {
        runGeneratedTest (makeIdFromType<Nullable<int>> id) ""
    }

    test "Compile for Int64" {
        runGeneratedTest (makeIdFromType<int64> id) ""
    }

    test "Compile for String" {
        runGeneratedTest (makeIdFromType<string> id) ""
    }

    test "Compile for Guid" {
        runGeneratedTest (makeIdFromType<Guid> id) ""
    }

    test "Compile for Exception" {
        runGeneratedTest (makeIdFromType<Exception> id) ""
    }

    test "Default property name" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        var instance = new Id(""test"");
        Check.That(instance.Value).IsEqualTo(""test"");
        "
    }

    test "Custom property name" {
        let idType = makeIdFromType<string> (fun i ->
                { i with ValueProperty = "SomeName" }
            )

        runGeneratedTest idType @"
        var instance = new Id(""test"");
        Check.That(instance.SomeName).IsEqualTo(""test"");
        "
    }

    test "Custom type name" {
        let idType = makeIdFromType<string> (fun i ->
                { i with Name = "SomeName" }
            )

        runGeneratedTest idType @"
        var instance = new SomeName(""test"");
        Check.That(instance.Value).IsEqualTo(""test"");
        "
    }

    test "Custom simple namespace" {
        let idType = makeIdFromType<string> (fun i ->
                { i with Namespace = "SomeNs" }
            )

        runGeneratedTest idType @"
        var instance = new SomeNs.Id(""test"");
        Check.That(instance.Value).IsEqualTo(""test"");
        "
    }

    test "Custom complex namespace" {
        let idType = makeIdFromType<string> (fun i ->
                { i with Namespace = "Some.Ns.For.Tests" }
            )

        runGeneratedTest idType @"
        var instance = new Some.Ns.For.Tests.Id(""test"");
        Check.That(instance.Value).IsEqualTo(""test"");
        "
    }

    test "Null string throw" {
        let idType = makeIdFromType<string> id

        Expect.throwsT<ArgumentNullException> (fun () -> runGeneratedTest idType @"new Id(null);") "throw with null arg"
    }

    test "Null string can be allowed" {
        let idType = makeIdFromType<string> (fun i ->
                { i with AllowNull = true }
            )

        runGeneratedTest idType @"
        var instance = new Id(null);
        Check.That(instance.Value).IsEqualTo(null);
        "
    }

    test "Value is interned" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        var instance = new Id(""Some_string_"");
        Check.That(string.IsInterned(instance.Value)).IsNotNull();
        "
    }

    test "ToString return the same string" {
        let idType = makeIdFromType<string> id

        runGeneratedTest idType @"
        var str = string.Intern(""Test"");
        var instance = new Id(str);
        Check.That(instance.ToString()).IsSameReferenceThan(str);
        "
    }

    test "Null ToString is empty string" {

        let idType = makeIdFromType<Nullable<int>> id
    
        runGeneratedTest idType @"
        var instance = new Id(null);
        Check.That(instance.ToString()).IsEqualTo("""");
        "
    }

    test "ToString is lifted" {
        let idType = makeIdFromType<Guid> id

        runGeneratedTest idType @"
        var guid = Guid.NewGuid();
        var instance = new Id(guid);
        Check.That(instance.ToString()).IsEqualTo(guid.ToString());
        "
    }

    test "IFormattable is lifted" {
        let idType = makeIdFromType<double> id

        runGeneratedTest idType @"
        var dbl = 4.2;
        var instance = new Id(dbl);
    
        Check.That(instance.ToString(""0.000""))
            .IsEqualTo(dbl.ToString(""0.000""));

        Check.That(instance.ToString(""0.000"", CultureInfo.InvariantCulture))
            .IsEqualTo(dbl.ToString(""0.000"", CultureInfo.InvariantCulture));

        var formattable = instance as IFormattable;
        Check.That(formattable).IsNotNull();
        Check.That(formattable.ToString(""0.000"", CultureInfo.InvariantCulture))
            .IsEqualTo(dbl.ToString(""0.000"", CultureInfo.InvariantCulture));
        "

    }

    test "Check is called on creation" {
        let idType = makeIdFromType<int> id

        runGeneratedTest' idType @"
        Check.ThatCode(() => new Id(-1)).Throws<InvalidOperationException>();
        Check.ThatCode(() => new Id(0)).DoesNotThrow();
        " @"
        partial struct Id
        {
            partial void CheckValue(int value)
            {
                if (value < 0)
                {
                    throw new InvalidOperationException(""Nope"");
                }
            }
        }
        "
    }
]

[<Tests>]
let test = testList "Equality" tests
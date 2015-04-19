module BlackFox.Stidgen.CsharpGenerationTests

open BlackFox.Stidgen.CsharpGeneration
open BlackFox.Stidgen.Description
open NFluent
open NUnit.Framework

[<Test>]
let ``string`` () =
    let idType = makeIdType<string> (fun i ->
            { i with
                Namespace = "BlackFox.Tests"
                AllowNull = false
                CastFromUnderlying = None
                CastToUnderlying = None
            }
        )

    let generated = idType |> idTypeToString
    let expected = """using System;

namespace BlackFox.Tests
{
    public partial class Id
    {
        public System.String Value { get; private set; }

        public Id(System.String value)
        {
            if (value == null)
            {
                throw new System.ArgumentNullException("value");
            }

            this.Value = value;
        }

        public override string ToString()
        {
            return this.Value.ToString();
        }
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore

[<Test>]
let ``string allow null`` () =
    let idType = makeIdType<string> (fun i ->
            { i with
                Namespace = "BlackFox.Tests"
                AllowNull = true
                CastFromUnderlying = None
                CastToUnderlying = None
            }
        )

    let generated = idType |> idTypeToString
    let expected = """using System;

namespace BlackFox.Tests
{
    public partial class Id
    {
        public System.String Value { get; private set; }

        public Id(System.String value)
        {
            this.Value = value;
        }

        public override string ToString()
        {
            if (this.Value == null)
            {
                return "";
            }

            return this.Value.ToString();
        }
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore

[<Test>]
let ``no namespace`` () =
    let idType = makeIdType<string> (fun i ->
            { i with
                Namespace = ""
                AllowNull = false
                CastFromUnderlying = None
                CastToUnderlying = None
            }
        )

    let generated = idType |> idTypeToString
    let expected = """using System;

public partial class Id
{
    public System.String Value { get; private set; }

    public Id(System.String value)
    {
        if (value == null)
        {
            throw new System.ArgumentNullException("value");
        }

        this.Value = value;
    }

    public override string ToString()
    {
        return this.Value.ToString();
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore

[<Test>]
let ``casts`` () =
    let idType = makeIdType<string> (fun i ->
            { i with
                CastFromUnderlying = Implicit
                CastToUnderlying = Explicit
            }
        )

    let generated = idType |> idTypeToString
    let expected = """using System;

public partial class Id
{
    public System.String Value { get; private set; }

    public Id(System.String value)
    {
        if (value == null)
        {
            throw new System.ArgumentNullException("value");
        }

        this.Value = value;
    }

    public override string ToString()
    {
        return this.Value.ToString();
    }

    public static implicit operator Id(System.String x)
    {
        return new Id(x);
    }

    public static explicit operator System.String(Id x)
    {
        return x.Value;
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore
module BlackFox.Stidgen.CsharpGenerationTests

open BlackFox.Stidgen.CsharpGeneration
open BlackFox.Stidgen.Description
open NFluent
open NUnit.Framework

[<Test>]
let ``string`` () =
    let idType = makeIdFromType<string> (fun i ->
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
        public string Value { get; private set; }

        public Id(string value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }

            Value = value;
        }

        public override string ToString()
        {
            return Value.ToString();
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore

[<Test>]
let ``string allow null`` () =
    let idType = makeIdFromType<string> (fun i ->
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
        public string Value { get; private set; }

        public Id(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            if (Value == null)
            {
                return "";
            }

            return Value.ToString();
        }

        public override int GetHashCode()
        {
            if (Value == null)
            {
                return 0;
            }

            return Value.GetHashCode();
        }
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore

[<Test>]
let ``no namespace`` () =
    let idType = makeIdFromType<string> (fun i ->
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
    public string Value { get; private set; }

    public Id(string value)
    {
        if (value == null)
        {
            throw new ArgumentNullException("value");
        }

        Value = value;
    }

    public override string ToString()
    {
        return Value.ToString();
    }

    public override int GetHashCode()
    {
        return Value.GetHashCode();
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore

[<Test>]
let ``casts`` () =
    let idType = makeIdFromType<string> (fun i ->
            { i with
                CastFromUnderlying = Implicit
                CastToUnderlying = Explicit
            }
        )

    let generated = idType |> idTypeToString
    let expected = """using System;

public partial class Id
{
    public string Value { get; private set; }

    public Id(string value)
    {
        if (value == null)
        {
            throw new ArgumentNullException("value");
        }

        Value = value;
    }

    public override string ToString()
    {
        return Value.ToString();
    }

    public override int GetHashCode()
    {
        return Value.GetHashCode();
    }

    public static implicit operator Id(string x)
    {
        return new Id(x);
    }

    public static explicit operator string (Id x)
    {
        return x.Value;
    }
}"""
    Check.That(generated).IsEqualTo<string>(expected) |> ignore
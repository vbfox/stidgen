module BlackFox.Stidgen.CsharpGenerationTests

open BlackFox.Stidgen.CsharpGeneration
open BlackFox.Stidgen.Description
open NFluent
open NUnit.Framework

[<Test>]
let ``string allow null`` () =
    let idType : IdType =
        {
            Name = "TestId";
            Namespace = "BlackFox.Tests"
            Type = typedefof<string>;
            ValueProperty = "Value"
            Visibility = Public
            AllowNull = true
        }

    let generated = idType |> idTypeToString
    let expected = """using System;

namespace BlackFox.Tests
{
    public partial class TestId
    {
        public System.String Value { get; private set; }

        public TestId(System.String value)
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
let ``string not allow null`` () =
    let idType : IdType =
        {
            Name = "TestId";
            Namespace = "BlackFox.Tests"
            Type = typedefof<string>;
            ValueProperty = "Value"
            Visibility = Public
            AllowNull = false
        }

    let generated = idType |> idTypeToString
    let expected = """using System;

namespace BlackFox.Tests
{
    public partial class TestId
    {
        public System.String Value { get; private set; }

        public TestId(System.String value)
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
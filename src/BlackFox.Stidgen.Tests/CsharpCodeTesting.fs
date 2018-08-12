module BlackFox.Stidgen.CsharpCodeTesting

open BlackFox.Stidgen.CsharpGeneration
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open System
open System.IO
open System.Reflection

type CompilationFailedException(message:string) = inherit Exception(message)

let private metadataRef<'t> = MetadataReference.CreateFromFile(typeof<'t>.Assembly.Location)

let private createCompilation (code:string seq) =
    let syntaxTrees = code |> Seq.map(fun c -> CSharpSyntaxTree.ParseText(c))

    let assemblyName = Path.GetRandomFileName();
    
    let (references : MetadataReference[]) =
        [|
            metadataRef<obj>
            metadataRef<System.Linq.Enumerable>
            metadataRef<System.CodeDom.Compiler.GeneratedCodeAttribute>
            metadataRef<NFluent.Check>
        |]

    let options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
    CSharpCompilation.Create(assemblyName, syntaxTrees, references, options)

let private loadCsharpCode code =
    let compilation = createCompilation code

    use ms = new MemoryStream()
    let result = compilation.Emit(ms);

    if not result.Success then
        let isError (diagnostic:Diagnostic) = diagnostic.IsWarningAsError || diagnostic.Severity = DiagnosticSeverity.Error
        let failures = result.Diagnostics |> Seq.filter isError
        let failures = failures |> Seq.map (fun d -> sprintf "%s: %s" d.Id (d.GetMessage()))
        let failures = System.String.Join("\r\n", failures)

        let codeSources =
            code
            |> Seq.mapi(fun i text ->
                let indented =
                    text.Split([|"\r\n"; "\r"; "\n"|], StringSplitOptions.None)
                    |> Seq.map (sprintf "\t%s")
                    |> (fun x -> String.Join("\r\n",x))
                sprintf "Source code %i:\r\n%s" i indented)
            |> (fun x -> String.Join("\r\n\r\n",x))

        raise (CompilationFailedException (sprintf "Test compilation failed\r\n\r\n%s\r\n\r\n%s" failures codeSources))
    else
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        Assembly.Load(ms.ToArray());

type TestClassName = |TestClassName of string
type TestMethodName = |TestMethodName of string
type TestMethodContent = |TestMethodContent of string
type OtherClasses = |OtherClasses of string
type TestTemplate = TestClassName -> TestMethodName -> TestMethodContent -> string
type TestCode = { ClassName: TestClassName; Code: string }

let private wrapTestCode code testMethodName (template:TestTemplate) =
    let nameGuid = Guid.NewGuid().ToByteArray() |> Seq.map( fun b -> b.ToString("X2"))
    let name = System.String.Join("", nameGuid)
    let name = sprintf "CodeGen_UnitTest_%s" name
    let name = TestClassName name
    { ClassName=name; Code=template name testMethodName code }

let private genericTemplate otherCode name testMethodName testCode = 
    let (TestClassName name) =  name
    let (TestMethodName testMethodName) =  testMethodName
    let (TestMethodContent testCode) =  testCode
    let print = sprintf "using System;
using System;
using System.Globalization;
using System.Collections.Generic;
using NFluent;

public static class %s
{
    public static void %s()
    {
        %s
    }
}

%s
"
    print name testMethodName testCode otherCode

let private singleMethodTemplate = genericTemplate ""

let private runGeneratedTestCore idType test template =
    let idTypeCode = idTypesToString [idType]
    let { Code=testCode; ClassName=TestClassName(className) } =
        wrapTestCode (TestMethodContent test) (TestMethodName "RunTest") template
    let assembly = loadCsharpCode [idTypeCode; testCode]

    let type' = assembly.GetType(className)
    let method' = type'.GetMethod("RunTest")
    try
        method'.Invoke(null, null) |> ignore
    with
    | :? System.Reflection.TargetInvocationException as ex ->
        // Remove the TargetInvocationException keeping the stacktrace
        let edi = System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex.InnerException)
        edi.Throw()

let runGeneratedTest' idType test otherCode = runGeneratedTestCore idType test (genericTemplate otherCode)

let runGeneratedTest idType test = runGeneratedTestCore idType test singleMethodTemplate
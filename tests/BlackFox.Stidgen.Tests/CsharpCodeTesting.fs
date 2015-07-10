module BlackFox.Stidgen.CsharpCodeTesting

open BlackFox.Stidgen.CsharpGeneration
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open System
open System.IO
open System.Reflection

let private metadataRef<'t> = MetadataReference.CreateFromFile(typeof<'t>.Assembly.Location)

let private createCompilation (code:string seq) =
    let syntaxTrees = code |> Seq.map(fun c -> CSharpSyntaxTree.ParseText(c))

    let assemblyName = Path.GetRandomFileName();
    
    let (references : MetadataReference[]) =
        [|
            metadataRef<obj>
            metadataRef<System.Linq.Enumerable>
            metadataRef<System.CodeDom.Compiler.GeneratedCodeAttribute>
            metadataRef<NUnit.Framework.Assert>
            metadataRef<NFluent.Check>
        |]

    let options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
    CSharpCompilation.Create(assemblyName, syntaxTrees, references, options);

let private loadCsharpCode code =
    let compilation = createCompilation code

    use ms = new MemoryStream()
    let result = compilation.Emit(ms);

    if not result.Success then
        let isError (diagnostic:Diagnostic) = diagnostic.IsWarningAsError || diagnostic.Severity = DiagnosticSeverity.Error
        let failures = result.Diagnostics |> Seq.filter isError
        let failures = failures |> Seq.map (fun d -> sprintf "%s: %s" d.Id (d.GetMessage()))
        let failures = System.String.Join(", ", failures)

        failwith ("Compilation failed -> " + failures)
    else
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        Assembly.Load(ms.ToArray());

let private wrapTestCode code =
    let name = Guid.NewGuid().ToByteArray() |> Seq.map( fun b -> b.ToString("X2"))
    let name = System.String.Join("", name)
    let name = sprintf "CodeGen_UnitTest_%s" name
    let template =sprintf "using System;
using System;
using System.Collections.Generic;
using NFluent;
using NUnit.Framework;

public static class %s
{
    public static void RunTest()
    {
        %s
    }
}
"
    template name code, name

let runGeneratedTest idType test =
    let idTypeCode = idTypesToString [idType]
    let testCode, testName = wrapTestCode test
    let assembly = loadCsharpCode [idTypeCode; testCode]

    let type' = assembly.GetType(testName)
    let method' = type'.GetMethod("RunTest")
    try
        method'.Invoke(null, null) |> ignore
    with
    | :? System.Reflection.TargetInvocationException as ex ->
        // Remove the TargetInvocationException keeping the stacktrace
        let edi = System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex.InnerException)
        edi.Throw()
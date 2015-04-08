module BlackFox.Stidgen.CsharpGeneration

open BlackFox.Stidgen.Description
open BlackFox.Stidgen.FluentRoslyn
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting
open System.IO
open System.Text

let private visibilityToKeyword = function
    | Public -> SyntaxKind.PublicKeyword
    | Private -> SyntaxKind.PrivateKeyword
    | Protected -> SyntaxKind.ProtectedKeyword

let toCompilationUnit idType = 
    let compilationUnit = SyntaxFactory.CompilationUnit().AddUsings("System")
 
    let generatedNamespace = SyntaxFactory.NamespaceDeclaration(SyntaxFactory.IdentifierName(idType.Namespace))
    
    let visibility = visibilityToKeyword idType.Visibility

    let generatedClass =
        SyntaxFactory.ClassDeclaration(idType.Name)
            .AddModifiers(SyntaxFactory.Token(visibility))
            .AddModifiers(SyntaxFactory.Token(SyntaxKind.PartialKeyword))
 
    let valueProperty = 
        SyntaxFactory.PropertyDeclaration(SyntaxFactory.ParseTypeName(idType.Type.FullName), idType.ValueProperty)
            .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
            .AddAccessorListAccessors(
                SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                    .WithSemicolonToken(),
                SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                    .AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword))
                    .WithSemicolonToken()
                )

    let generatedMethod =
        SyntaxFactory.MethodDeclaration(SyntaxFactory.ParseTypeName("void"), "Test")
            .AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword))
            .AddModifiers(SyntaxFactory.Token(SyntaxKind.StaticKeyword))
            .WithBody(SyntaxFactory.Block())
 
    let rootClass = generatedClass.AddMembers(generatedMethod, valueProperty)
    let ns = generatedNamespace.AddMembers(rootClass)

    compilationUnit.AddMembers(ns)

let compilationUnitToString compilationUnit =
    let stringBuilder = new StringBuilder()
    let workspace = new AdhocWorkspace()
    let formatted = Formatter.Format(compilationUnit, workspace)
    use writer = new StringWriter(stringBuilder)
    formatted.WriteTo(writer)
    
    writer.ToString()

let gen idType =
    idType
        |> toCompilationUnit
        |> compilationUnitToString
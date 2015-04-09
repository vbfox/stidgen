module BlackFox.Stidgen.CsharpGeneration

open System
open System.IO
open System.Text
open BlackFox.Stidgen.Description
open BlackFox.Stidgen.FluentRoslyn
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Simplification

type private ParsedInfo =
    {
        TypeSyntax : TypeSyntax
    }

let private (|?>) x (c, f) = if c then f x else x
let private (|??>) x (c, f, g) = if c then f x else g x

let private visibilityToKeyword = function
    | Public -> SyntaxKind.PublicKeyword
    | Private -> SyntaxKind.PrivateKeyword
    | Protected -> SyntaxKind.ProtectedKeyword

let private makeValueProperty info idType =
    SyntaxFactory.PropertyDeclaration(info.TypeSyntax, idType.ValueProperty)
        .AddAccessorListAccessors(
            SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                |> withSemicolon,
            SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                |> addModifiers [|SyntaxKind.PrivateKeyword|]
                |> withSemicolon
            )
        |> addModifiers [|SyntaxKind.PublicKeyword|]

let private firstCharToLower (x:string) = 
    if x.Length = 0 then
        x
    else
        let first = System.Char.ToLowerInvariant(x.[0])
        first.ToString() + x.Substring(1)

let private typenameof<'t> = SyntaxFactory.ParseTypeName(typedefof<'t>.FullName)

let private makeCtor info idType =
    let argName = firstCharToLower idType.ValueProperty

    let checkForNull = 
        SyntaxFactory.IfStatement(
            SyntaxFactory.BinaryExpression(
                SyntaxKind.EqualsExpression,
                SyntaxFactory.IdentifierName(argName),
                SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
            ),
            SyntaxFactory.Block(
                SyntaxFactory.ThrowStatement(
                    SyntaxFactory.ObjectCreationExpression(typenameof<ArgumentNullException>)
                    |> addArgument (stringLiteral argName)
                )
            )
        )

    let assignProperty =
        setThisMember idType.ValueProperty (SyntaxFactory.IdentifierName(argName))

    SyntaxFactory.ConstructorDeclaration(idType.Name)
    |> addModifiers [|SyntaxKind.PublicKeyword|]
    |> addParameter argName info.TypeSyntax
    |?> (not idType.AllowNull, addBodyStatement checkForNull)
    |> addBodyStatement assignProperty

let private makeToString info idType =
    let returnToString =
        SyntaxFactory.ReturnStatement(
            SyntaxFactory.InvocationExpression(
                thisMemberAccess idType.ValueProperty |> memberAccess "ToString"
            )
        )

    let returnIfNull =
        SyntaxFactory.IfStatement(
            SyntaxFactory.BinaryExpression(
                SyntaxKind.EqualsExpression,
                thisMemberAccess idType.ValueProperty,
                nullLiteral),
            SyntaxFactory.Block()
                |> addStatement (SyntaxFactory.ReturnStatement(stringLiteral ""))
        )

    SyntaxFactory.MethodDeclaration(stringTypeSyntax, "ToString")
    |> addModifiers [|SyntaxKind.PublicKeyword; SyntaxKind.OverrideKeyword|]
    |?> (idType.AllowNull, addBodyStatement returnIfNull)
    |> addBodyStatement returnToString

let private makeClass idType info = 
    let visibility = visibilityToKeyword idType.Visibility
    let generatedClass =
        SyntaxFactory.ClassDeclaration(idType.Name)
            |> addModifiers [|visibility; SyntaxKind.PartialKeyword|]
 
    generatedClass.AddMembers(
        idType |> makeValueProperty info,
        idType |> makeCtor info,
        idType |> makeToString info
    )

let toCompilationUnit idType = 
    let info =
        {
            TypeSyntax = SyntaxFactory.ParseTypeName(idType.Type.FullName)
        }

    let generatedNamespace =
        SyntaxFactory.NamespaceDeclaration(SyntaxFactory.IdentifierName(idType.Namespace))
            .AddMembers(makeClass idType info)
            .WithAdditionalAnnotations(Simplifier.Annotation)

    SyntaxFactory.CompilationUnit()
        .AddUsings("System")
        .AddMembers(generatedNamespace)

let compilationUnitToString compilationUnit =
    let stringBuilder = new StringBuilder()
    let workspace = new AdhocWorkspace()
    let formatted = Formatter.Format(compilationUnit, workspace)
    use writer = new StringWriter(stringBuilder)
    formatted.WriteTo(writer)
    
    writer.ToString()

let idTypeToString idType =
    idType
        |> toCompilationUnit
        |> compilationUnitToString
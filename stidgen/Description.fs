module BlackFox.Stidgen.Description

open System

type Visibility =
    | Private
    | Public
    | Protected

type Cast =
    | None
    | Implicit
    | Explicit

type IdType =
    {
        Name : string
        Namespace : string
        Type : Type
        ValueProperty : string
        Visibility : Visibility
        AllowNull : bool
        CastFromUnderlying : Cast
        CastToUnderlying : Cast
    }

let makeIdType (targetType:Type) (idTypeBuilder : IdType -> IdType) = 
    let idType = 
        {
            Name = "Id";
            Namespace = ""
            Type = targetType;
            ValueProperty = "Value"
            Visibility = Public
            AllowNull = false
            CastFromUnderlying = Explicit
            CastToUnderlying = Explicit
        }

    idTypeBuilder idType

let makeIdFromType<'t> (idTypeBuilder : IdType -> IdType) =
    makeIdType typedefof<'t> idTypeBuilder
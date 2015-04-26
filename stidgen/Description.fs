module BlackFox.Stidgen.Description

open System

type Visibility =
    | Private
    | Public
    | Protected

/// How should a cast be generated
type Cast =
    /// Do not generate any cast
    | None

    /// Generate an implicit cast
    | Implicit

    /// Generate an explicit cast
    | Explicit

/// An "id type" that can be generated
type IdType =
    {
        /// Name of the id type
        Name : string

        /// Namespace of the id type
        Namespace : string

        /// Underlying type
        Type : Type

        /// Name of the "value" property
        ValueProperty : string

        /// Visibility of the id type
        Visibility : Visibility

        /// If the underlying type is nullable, is null a valid value for the id type
        AllowNull : bool

        /// Cast to be generated from the underlying type to the id type
        CastFromUnderlying : Cast

        /// Cast to be generated from the id type to the underlying type
        CastToUnderlying : Cast

        /// Are the underlying and id types considered equals for Object.Equals, and equality operators
        EqualsUnderlying : bool
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
            EqualsUnderlying = false
        }

    idTypeBuilder idType

let makeIdFromType<'t> (idTypeBuilder : IdType -> IdType) =
    makeIdType typedefof<'t> idTypeBuilder
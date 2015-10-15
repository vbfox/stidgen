module BlackFox.Stidgen.Description

open System

/// Visibility of the created class
type ClassVisibility =
    | Public
    | Internal

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
        UnderlyingType : Type

        /// Name of the "value" property
        ValueProperty : string

        /// Visibility of the id type
        Visibility : ClassVisibility

        /// If the underlying type is nullable, is null a valid value for the id type
        AllowNull : bool

        /// Cast to be generated from the underlying type to the id type
        CastFromUnderlying : Cast

        /// Cast to be generated from the id type to the underlying type
        CastToUnderlying : Cast

        /// Are the underlying and id types considered equals for Object.Equals, and equality operators
        EqualsUnderlying : bool

        /// If the underlying type is string, are the values interned before being stored
        InternString : bool

        /// Specify if the name of the ID type should be used as the name of the file.
        /// By default the code is generated in a file named as the .stidgen one.
        UseNameAsFileName : bool

        /// Name of the generated file
        FileName : string option

        /// Enable generation of the protobuf-net attributes necessary for serialization
        ProtobufnetSerializable : bool

        /// Enable generation of the DataContract attributes necessary for serialization
        DataContractSerializable : bool
    }

/// Create an IdType with the default parameters for the targetType
let makeIdType (targetType:Type) (idTypeBuilder : IdType -> IdType) = 
    let idType = 
        {
            Name = "Id";
            Namespace = ""
            UnderlyingType = targetType;
            ValueProperty = "Value"
            Visibility = Public
            AllowNull = false
            CastFromUnderlying = Explicit
            CastToUnderlying = Explicit
            EqualsUnderlying = false
            InternString = true
            UseNameAsFileName = false
            FileName = Option.None
            ProtobufnetSerializable = false
            DataContractSerializable = false
        }

    idTypeBuilder idType

/// Create an IdType with the default parameters for the type parameter
let makeIdFromType<'t> (idTypeBuilder : IdType -> IdType) =
    makeIdType typeof<'t> idTypeBuilder
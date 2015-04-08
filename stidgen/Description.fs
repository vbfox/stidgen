module BlackFox.Stidgen.Description

open System

type Visibility =
    | Private
    | Public
    | Protected

type IdType =
    {
        Name : string
        Namespace : string
        Type : Type
        ValueProperty : string
        Visibility : Visibility
    }

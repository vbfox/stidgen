Strongly Typed ID type Generator
================================

[![Github Actions Status](https://github.com/vbfox/stidgen/actions/workflows/main.yml/badge.svg)](https://github.com/vbfox/stidgen/actions/workflows/main.yml?query=branch%3Amaster)
[![Nuget Package](https://img.shields.io/nuget/v/stidgen.svg)](https://www.nuget.org/packages/stidgen)

This tool uses simple text files as input like :

	public Corp.Product.UserId<string>

    public Corp.Product.TweetId<long>
        EqualsUnderlying: true

And generate for each type specified a type that can be used as a strongly-typed ID:

* It's an immutable value type with a single member.
* The `struct` is partial to allow addition of methods and properties.
* `null` values are disallowed by default and an extensibility point is available to add more application-dependent checks.
* Casts from and to the underlying type are available. (explicit by default)
* Equality member and operators are lifted, by default the ID types are only equals between themselves but equality with the underlying type is can be enabled with `EqualsUnderlying` setting.
* `IEquatable<T>` is implemented.
* `IConvertible` is implemented if the underlying type implement it.
* `IComparable<Id>` is implemented if `IComparable<Underlying>` is implemented by the underlying type.
* Other `IComparable<T>` implementations are lifted if `EqualsUnderlying` is set.
* `string` IDs are interned by default.

Examples of generated files can be found in the [sample folder](https://github.com/vbfox/stidgen/tree/master/samples).

Installation
------------

The recommended way is via paket or nuget.

For nuget :

    nuget install stidgen -ExcludeVersion -OutputDirectory packages

For paket, add `nuget stidgen` to your `paket.dependencies` and then to
install it :

    /.paket/paket.exe install

A **zip** file containing the latest released version can also be found in
the [latest GitHub release](https://github.com/vbfox/stidgen/releases/latest).

Usage
-----

Then to generate all files specified by a stidgen definition file :

    ./packages/stidgen/tools/stidgen.exe myfile.stidgen

Definition file format
----------------------

Each definition file can contain a number of types of the form :

    public|internal [Namespace.]TypeName<UnderlyingType>
        PropertyA: value
        PropertyB: value

### Properties :

* **ValueProperty** (string): Specify the name of the property containing the underlying value. Default to "Value".
* **AllowNull** (bool): Allow null underlying values (only applies if the underlying type is a reference type). Default to false.
* **InternString** (bool): Intern string underlying values (only applies if the underlying type is `string`. Default to true.
* **EqualsUnderlying** (bool): Make the underlying be equal to the Id type when Equals, CompareTo and == are called.<br/>
Also implements `IEquatable<Underlying>` and `IComparable<Underlying>` on the id type.
* **CastToUnderlying** ("explicit" or "implicit"): Specify how is generated the cast from the id type to the underlying type. Default to "explicit".
* **CastFromUnderlying** ("explicit" or "implicit"): Specify how is generated the cast from the underlying type to the id type. Default to "explicit".
* **FileName** (string): Full name of the file to generate for this type. Default to "TypeName.Generated.cs".
* **ProtobufnetSerializable** (bool): Enable the generation of `[ProtoContract]` and `[ProtoMember]` attributes for [protobuf-net](https://github.com/mgravell/protobuf-net) support. Default to false.


[LicenseBadge]: https://img.shields.io/badge/license-MIT%20License-blue.svg

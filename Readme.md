Strongly Typed ID type Generator
-------------------------------

[![Join the chat at https://gitter.im/vbfox/stidgen](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/vbfox/stidgen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Build status](https://ci.appveyor.com/api/projects/status/6ehdd4cam628ve57/branch/master?svg=true)](https://ci.appveyor.com/project/vbfox/stidgen/branch/master)
[![MIT License](https://img.shields.io/github/license/vbfox/stidgen.svg)](LICENSE.md)
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
* Equality member and operators are lifted, by default the ID types are only equals between themselves but equality with the underlying type is optionally available.
* `IEquatable<T>` is implemented.
* `IConvertible` is implemented if the underlying type implement it.
* `string` IDs are interned by default.

Installation
------------

For now only via NuGet :

    nuget install stidgen -ExcludeVersion -OutputDirectory packages


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
* **EqualsUnderlying** (bool): Make the underlying be equal to the Id type when Equals and == are called.
* **CastToUnderlying** ("explicit" or "implicit"): Specify how is generated the cast from the id type to the underlying type. Default to "explicit".
* **CastFromUnderlying** ("explicit" or "implicit"): Specify how is generated the cast from the underlying type to the id type. Default to "explicit".
* **FileName** (string): Full name of the file to generate for this type. Default to "TypeName.Generated.cs".
* **ProtobufnetSerializable** (bool): Enable the generation of `[ProtoContract]` and `[ProtoMember]` attributes for [protobuf-net](https://github.com/mgravell/protobuf-net) support. Default to false.
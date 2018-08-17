//----------------------
// <auto-generated>
//     Generated by stidgen
// </auto-generated>
//----------------------
using System;
using System.CodeDom.Compiler;
using System.Diagnostics;
using System.Runtime.CompilerServices;

[DebuggerDisplay("{value,nq}")]
public partial struct UserId : IEquatable<UserId>, IComparable<UserId>, IConvertible
{
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    private string value;

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public string Value
    {
        get
        {
            return value;
        }
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public UserId(string value)
    {
        if (value == null)
        {
            throw new ArgumentNullException("value");
        }

        this.value = string.Intern(value);
        CheckValue(this.value);
    }

    partial void CheckValue(string value);

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override string ToString()
    {
        return value;
    }

    /// <summary>Serves as the default hash function.</summary>
    /// <returns>A hash code for the current object.</returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override int GetHashCode()
    {
        return RuntimeHelpers.GetHashCode(value);
    }

    /// <summary>
    /// Determines whether the specified object is equal to the current object.
    /// <param name="other">The object to compare with the current object.</param>
    /// </summary>
    /// <returns>
    /// true if the specified object is equal to the current object;
    /// otherwise, false.
    /// </returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override bool Equals(object other)
    {
        Debug.Assert(value == null || string.IsInterned(value) == null, "Value should always be interned if interning is enabled");
        if (!(other is UserId))
        {
            return false;
        }

        return object.ReferenceEquals(value, ((UserId)other).value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public bool Equals(UserId other)
    {
        Debug.Assert(value == null || string.IsInterned(value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(other.value == null || string.IsInterned(other.value) == null, "Value should always be interned if interning is enabled");
        return object.ReferenceEquals(value, other.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool Equals(UserId a, UserId b)
    {
        Debug.Assert(a.value == null || string.IsInterned(a.value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(b.value == null || string.IsInterned(b.value) == null, "Value should always be interned if interning is enabled");
        return object.ReferenceEquals(a.value, b.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool operator ==(UserId left, UserId right)
    {
        Debug.Assert(left.value == null || string.IsInterned(left.value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(right.value == null || string.IsInterned(right.value) == null, "Value should always be interned if interning is enabled");
        return object.ReferenceEquals(left.value, right.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool operator !=(UserId left, UserId right)
    {
        Debug.Assert(left.value == null || string.IsInterned(left.value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(right.value == null || string.IsInterned(right.value) == null, "Value should always be interned if interning is enabled");
        return !object.ReferenceEquals(left.value, right.value);
    }

    /// <summary>
    /// Compares the current instance with another object of the same type and
    /// returns an integer that indicates whether the current instance precedes,
    /// follows, or occurs in the same position in the sort order as the other
    /// object.
    /// </summary>
    /// <param name="other">An object to compare with this instance.</param>
    /// <returns>
    /// A value that indicates the relative order of the objects being compared.
    /// The return value has these meanings: Value Meaning Less than zero This
    /// instance precedes <paramref name="other" /> in the sort order.  Zero This
    /// instance occurs in the same position in the sort order as
    /// <paramref name="other" />. Greater than zero This instance follows
    /// <paramref name="other" /> in the sort order.
    /// </returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public int CompareTo(UserId other)
    {
        if (value == null)
            if (other.value == null)
                return 0;
            else
                return -1;
        return this.value.CompareTo(other.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator string(UserId x)
    {
        return x.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator UserId(string x)
    {
        return new UserId(x);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator string(UserId? x)
    {
        return !x.HasValue || x.Value.value == null ? null : x.Value.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator UserId? (string x)
    {
        return x == null ? (UserId?)null : new UserId(x);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    TypeCode IConvertible.GetTypeCode()
    {
        if (value == null)
        {
            return Convert.GetTypeCode(null);
        }

        return this.value.GetTypeCode();
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    bool IConvertible.ToBoolean(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToBoolean(null);
        }

        return ((IConvertible)value).ToBoolean(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    char IConvertible.ToChar(IFormatProvider provider)
    {
        if (provider == null)
        {
            throw new ArgumentNullException("provider");
        }

        return ((IConvertible)value).ToChar(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    sbyte IConvertible.ToSByte(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToSByte(null);
        }

        return ((IConvertible)value).ToSByte(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    byte IConvertible.ToByte(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToByte(null);
        }

        return ((IConvertible)value).ToByte(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    short IConvertible.ToInt16(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToInt16(null);
        }

        return ((IConvertible)value).ToInt16(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    ushort IConvertible.ToUInt16(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToUInt16(null);
        }

        return ((IConvertible)value).ToUInt16(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    int IConvertible.ToInt32(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToInt32(null);
        }

        return ((IConvertible)value).ToInt32(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    uint IConvertible.ToUInt32(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToUInt32(null);
        }

        return ((IConvertible)value).ToUInt32(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    long IConvertible.ToInt64(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToInt64(null);
        }

        return ((IConvertible)value).ToInt64(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    ulong IConvertible.ToUInt64(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToUInt64(null);
        }

        return ((IConvertible)value).ToUInt64(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    float IConvertible.ToSingle(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToSingle(null);
        }

        return ((IConvertible)value).ToSingle(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    double IConvertible.ToDouble(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToDouble(null);
        }

        return ((IConvertible)value).ToDouble(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    decimal IConvertible.ToDecimal(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToDecimal(null);
        }

        return ((IConvertible)value).ToDecimal(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    DateTime IConvertible.ToDateTime(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToDateTime(null);
        }

        return ((IConvertible)value).ToDateTime(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    string IConvertible.ToString(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToString(null);
        }

        return this.value.ToString(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    object IConvertible.ToType(Type conversionType, IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ChangeType(null, conversionType);
        }

        return ((IConvertible)value).ToType(conversionType, provider);
    }
}

[DebuggerDisplay("{value,nq}")]
public partial struct CompanyId : IEquatable<CompanyId>, IComparable<CompanyId>, IConvertible
{
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    private string value;

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public string Value
    {
        get
        {
            return value;
        }
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public CompanyId(string value)
    {
        if (value == null)
        {
            throw new ArgumentNullException("value");
        }

        this.value = string.Intern(value);
        CheckValue(this.value);
    }

    partial void CheckValue(string value);

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override string ToString()
    {
        return value;
    }

    /// <summary>Serves as the default hash function.</summary>
    /// <returns>A hash code for the current object.</returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override int GetHashCode()
    {
        return RuntimeHelpers.GetHashCode(value);
    }

    /// <summary>
    /// Determines whether the specified object is equal to the current object.
    /// <param name="other">The object to compare with the current object.</param>
    /// </summary>
    /// <returns>
    /// true if the specified object is equal to the current object;
    /// otherwise, false.
    /// </returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override bool Equals(object other)
    {
        Debug.Assert(value == null || string.IsInterned(value) == null, "Value should always be interned if interning is enabled");
        if (!(other is CompanyId))
        {
            return false;
        }

        return object.ReferenceEquals(value, ((CompanyId)other).value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public bool Equals(CompanyId other)
    {
        Debug.Assert(value == null || string.IsInterned(value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(other.value == null || string.IsInterned(other.value) == null, "Value should always be interned if interning is enabled");
        return object.ReferenceEquals(value, other.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool Equals(CompanyId a, CompanyId b)
    {
        Debug.Assert(a.value == null || string.IsInterned(a.value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(b.value == null || string.IsInterned(b.value) == null, "Value should always be interned if interning is enabled");
        return object.ReferenceEquals(a.value, b.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool operator ==(CompanyId left, CompanyId right)
    {
        Debug.Assert(left.value == null || string.IsInterned(left.value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(right.value == null || string.IsInterned(right.value) == null, "Value should always be interned if interning is enabled");
        return object.ReferenceEquals(left.value, right.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool operator !=(CompanyId left, CompanyId right)
    {
        Debug.Assert(left.value == null || string.IsInterned(left.value) == null, "Value should always be interned if interning is enabled");
        Debug.Assert(right.value == null || string.IsInterned(right.value) == null, "Value should always be interned if interning is enabled");
        return !object.ReferenceEquals(left.value, right.value);
    }

    /// <summary>
    /// Compares the current instance with another object of the same type and
    /// returns an integer that indicates whether the current instance precedes,
    /// follows, or occurs in the same position in the sort order as the other
    /// object.
    /// </summary>
    /// <param name="other">An object to compare with this instance.</param>
    /// <returns>
    /// A value that indicates the relative order of the objects being compared.
    /// The return value has these meanings: Value Meaning Less than zero This
    /// instance precedes <paramref name="other" /> in the sort order.  Zero This
    /// instance occurs in the same position in the sort order as
    /// <paramref name="other" />. Greater than zero This instance follows
    /// <paramref name="other" /> in the sort order.
    /// </returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public int CompareTo(CompanyId other)
    {
        if (value == null)
            if (other.value == null)
                return 0;
            else
                return -1;
        return this.value.CompareTo(other.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator string(CompanyId x)
    {
        return x.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator CompanyId(string x)
    {
        return new CompanyId(x);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator string(CompanyId? x)
    {
        return !x.HasValue || x.Value.value == null ? null : x.Value.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator CompanyId? (string x)
    {
        return x == null ? (CompanyId?)null : new CompanyId(x);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    TypeCode IConvertible.GetTypeCode()
    {
        if (value == null)
        {
            return Convert.GetTypeCode(null);
        }

        return this.value.GetTypeCode();
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    bool IConvertible.ToBoolean(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToBoolean(null);
        }

        return ((IConvertible)value).ToBoolean(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    char IConvertible.ToChar(IFormatProvider provider)
    {
        if (provider == null)
        {
            throw new ArgumentNullException("provider");
        }

        return ((IConvertible)value).ToChar(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    sbyte IConvertible.ToSByte(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToSByte(null);
        }

        return ((IConvertible)value).ToSByte(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    byte IConvertible.ToByte(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToByte(null);
        }

        return ((IConvertible)value).ToByte(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    short IConvertible.ToInt16(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToInt16(null);
        }

        return ((IConvertible)value).ToInt16(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    ushort IConvertible.ToUInt16(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToUInt16(null);
        }

        return ((IConvertible)value).ToUInt16(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    int IConvertible.ToInt32(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToInt32(null);
        }

        return ((IConvertible)value).ToInt32(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    uint IConvertible.ToUInt32(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToUInt32(null);
        }

        return ((IConvertible)value).ToUInt32(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    long IConvertible.ToInt64(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToInt64(null);
        }

        return ((IConvertible)value).ToInt64(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    ulong IConvertible.ToUInt64(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToUInt64(null);
        }

        return ((IConvertible)value).ToUInt64(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    float IConvertible.ToSingle(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToSingle(null);
        }

        return ((IConvertible)value).ToSingle(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    double IConvertible.ToDouble(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToDouble(null);
        }

        return ((IConvertible)value).ToDouble(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    decimal IConvertible.ToDecimal(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToDecimal(null);
        }

        return ((IConvertible)value).ToDecimal(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    DateTime IConvertible.ToDateTime(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToDateTime(null);
        }

        return ((IConvertible)value).ToDateTime(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    string IConvertible.ToString(IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ToString(null);
        }

        return this.value.ToString(provider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    object IConvertible.ToType(Type conversionType, IFormatProvider provider)
    {
        if (value == null)
        {
            return Convert.ChangeType(null, conversionType);
        }

        return ((IConvertible)value).ToType(conversionType, provider);
    }
}

[DebuggerDisplay("{value,nq}")]
public partial struct UploadedFileId : IEquatable<UploadedFileId>, IComparable<UploadedFileId>, IFormattable
{
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    private Guid value;

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public Guid Value
    {
        get
        {
            return value;
        }
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public UploadedFileId(Guid value)
    {
        this.value = value;
        CheckValue(this.value);
    }

    partial void CheckValue(Guid value);

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override string ToString()
    {
        return value.ToString();
    }

    /// <summary>Serves as the default hash function.</summary>
    /// <returns>A hash code for the current object.</returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override int GetHashCode()
    {
        return value.GetHashCode();
    }

    /// <summary>
    /// Determines whether the specified object is equal to the current object.
    /// <param name="other">The object to compare with the current object.</param>
    /// </summary>
    /// <returns>
    /// true if the specified object is equal to the current object;
    /// otherwise, false.
    /// </returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public override bool Equals(object other)
    {
        if (!(other is UploadedFileId))
        {
            return false;
        }

        return value == ((UploadedFileId)other).value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public bool Equals(UploadedFileId other)
    {
        return value == other.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool Equals(UploadedFileId a, UploadedFileId b)
    {
        return a.value == b.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool operator ==(UploadedFileId left, UploadedFileId right)
    {
        return left.value == right.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool operator !=(UploadedFileId left, UploadedFileId right)
    {
        return left.value != right.value;
    }

    /// <summary>
    /// Compares the current instance with another object of the same type and
    /// returns an integer that indicates whether the current instance precedes,
    /// follows, or occurs in the same position in the sort order as the other
    /// object.
    /// </summary>
    /// <param name="other">An object to compare with this instance.</param>
    /// <returns>
    /// A value that indicates the relative order of the objects being compared.
    /// The return value has these meanings: Value Meaning Less than zero This
    /// instance precedes <paramref name="other" /> in the sort order.  Zero This
    /// instance occurs in the same position in the sort order as
    /// <paramref name="other" />. Greater than zero This instance follows
    /// <paramref name="other" /> in the sort order.
    /// </returns>
    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public int CompareTo(UploadedFileId other)
    {
        return ((IComparable<Guid>)value).CompareTo(other.value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator Guid(UploadedFileId x)
    {
        return x.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator UploadedFileId(Guid x)
    {
        return new UploadedFileId(x);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator Guid? (UploadedFileId? x)
    {
        return !x.HasValue ? (Guid?)null : x.Value.value;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static explicit operator UploadedFileId? (Guid? x)
    {
        return !x.HasValue ? (UploadedFileId?)null : new UploadedFileId(x.Value);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static UploadedFileId Parse(string input)
    {
        return new UploadedFileId(Guid.Parse(input));
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static bool TryParse(string input, out UploadedFileId result)
    {
        Guid parsed;
        bool isValid = Guid.TryParse(input, out parsed);
        result = isValid ? new UploadedFileId(parsed) : default(UploadedFileId);
        return isValid;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public static UploadedFileId? TryParse(string input)
    {
        Guid parsed;
        bool isValid = Guid.TryParse(input, out parsed);
        return isValid ? new UploadedFileId(parsed) : (UploadedFileId?)null;
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public string ToString(string format, IFormatProvider formatProvider)
    {
        return ((IFormattable)value).ToString(format, formatProvider);
    }

    [GeneratedCode("BlackFox.Stidgen", "1.1.0")]
    public string ToString(string format)
    {
        return value.ToString(format, System.Globalization.CultureInfo.CurrentCulture);
    }
}
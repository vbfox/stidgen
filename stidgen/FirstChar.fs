module BlackFox.Stidgen.FirstChar

/// Apply a function to the first char
let apply (f : char -> char) (x:string) = 
    if x.Length = 0 then
        x
    else
        let first = f (x.[0])
        first.ToString() + x.Substring(1)

/// Make the first char lower-case
let toLower = apply System.Char.ToLowerInvariant

/// Make the first char upper-case
let toUpper = apply System.Char.ToUpperInvariant


module Caesar1 exposing (..)
 
lowercase_min = Char.toCode 'a'
uppercase_min = Char.toCode 'A'

encode: Int -> Char -> Char
encode shiftBy input =
    if not (Char.isAlpha input) then input -- Return non-letters unencoded.
    else
        let shiftedCode = Char.toCode input + shiftBy
            base = if Char.isUpper input then uppercase_min else lowercase_min
        in
            Char.fromCode (modBy 26 (shiftedCode - base) + base)

decode: Int -> Char -> Char
decode shiftBy input =  encode -shiftBy input

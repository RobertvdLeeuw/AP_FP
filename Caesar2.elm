module Caesar2 exposing (..)

import Caesar1 exposing (encode)

normalize: String -> String
normalize word =
    let
        normalizeLetter = \letter -> if Char.isAlpha letter then String.toLower (String.fromChar letter) else ""
    in
        String.concat (List.map normalizeLetter (String.toList word))

encrypt: Int -> String -> String
encrypt shift input =
    let
        normalized = String.toList (normalize input)
    in
        String.fromList (List.map (encode shift) normalized)


decrypt: Int -> String -> String
decrypt shift input = encrypt -shift input

module Caesar3 exposing (..)

import Caesar2 exposing (encrypt, normalize)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct l1 l2 =
    List.concatMap (\x -> List.map (\y -> (x, y)) l2) l1

checkSemantics: List String -> (Int, String) -> Bool
checkSemantics semantics (index, canary) =
    let contains = \can (sem, atIndex) -> (String.slice atIndex (atIndex + String.length sem) can) == sem
        normalizedSemantics = List.map normalize semantics  -- Since all of the previous exercises insisted on normalizing the input, the assumption has been made that the same must apply to the semantics.
        combinations = cartesianProduct normalizedSemantics (List.range 0 (String.length canary))
    in
        List.any (contains canary) combinations


candidates: List String -> String -> List (Int, String)
candidates semantics canaries =
    let shiftedCanaries = List.map (\i -> (i, encrypt i canaries)) (List.range 0 25)
    in
        List.filter (checkSemantics semantics) shiftedCanaries

module Msort exposing (..)


popLowest: List number -> List number -> (Maybe number, List number, List number)
popLowest l r =
    let lHead = List.head l
        rHead = List.head r

        lTail = Maybe.withDefault [] (List.tail l)
        rTail = Maybe.withDefault [] (List.tail r)
    in
        case (lHead, rHead) of
            (Nothing, Nothing) -> (Nothing, [], [])
            (Just x, Nothing) -> (lHead, lTail, [])
            (Nothing, Just y) -> (rHead, [], rTail)
            (Just x, Just y) -> if x < y then
                                    (lHead , lTail, r)
                                else (rHead, rTail, l)  -- Also handles l.head() == r.head().


merge: List number -> List number -> List number
merge l r =
    if List.isEmpty l then r else if List.isEmpty r then l else
        let (lowest, newL, newR) = popLowest l r
        in case lowest of
            Nothing -> []
            Just x -> [x] ++ merge newL newR

msort: List number -> List number
msort input =
    let halfwayIndex = (List.length input) // 2
        half1 = List.take halfwayIndex input
        half2 = List.drop halfwayIndex input
    in case (half1, half2) of
        ([], [y]) -> merge [] [y]  -- Since halfwayIndex is always rounded up, we never have a first half of length 1 and an empty second half.
        ([x], [y]) -> merge [x] [y]
        _ -> merge (msort half1) (msort half2)

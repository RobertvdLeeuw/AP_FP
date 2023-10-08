module Test exposing (..)

minimumGainA: List Float -> Maybe Float
minimumGainA profits =
    let positives = List.filter (\x -> x > 0) profits
        positiveMin = List.head (List.sort positives)
    in case positiveMin of
        Just x -> if x > 0 then Just x else Nothing
        Nothing -> Nothing

minimumGainB: List Float -> Maybe Float
minimumGainB profits =
    case profits of
        [] -> Nothing  -- Not used, but to catch all patterns.
        [x] -> if x > 0 then Just x else Nothing
        x :: xs ->
            let next = minimumGainB xs
            in case next of
                Nothing -> if x > 0 then Just x else Nothing
                Just y -> if y < x then Just y else Just x

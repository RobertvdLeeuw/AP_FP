module HigherOrder exposing (..)


repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil criterion func num =
    let result = func num
    in
        if result == num then num else  -- To avoid infinite loops.
            if criterion result then
                repeatUntil criterion func result
            else result

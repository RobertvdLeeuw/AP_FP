module Math exposing (..)

type Function
    = Const Int
    | X
    | Plus Function Function
    | Minus Function Function
    | Mult Function Function
    | Poly Function Int

print : Function -> String
print func =
    case func of
        Const n ->
            String.fromInt n

        X ->
            "x"

        Plus left right ->
            "(" ++ print left ++ " + " ++ print right ++ ")"

        Minus left right ->
            "(" ++ print left ++ " - " ++ print right ++ ")"

        Mult left right ->
            "(" ++ print left ++ " * " ++ print right ++ ")"

        Poly base exponent ->
            "(" ++ print base ++ " ^ " ++ String.fromInt exponent ++ ")"


-- eval: Float -> Function -> Float
-- eval x f = f x

rangeWithSteps: Float -> Float -> Float -> List Float
rangeWithSteps current max step = if current > max then [] else [current] ++ rangeWithSteps (current + step) max step

graph: (Int -> number) -> Int -> Int -> Int -> Int -> String
graph f xMin xMax yMin yMax =
    let width = 25
        length = 25

        sizePerChar = (toFloat yMax - yMin) / width

        values = List.map f (rangeWithSteps (toFloat xMin) (toFloat xMax) sizePerChar)


        clampedValues = List.map (clamp yMin yMax) values
        relativeValues = List.map (\x -> x - yMin) clampedValues


        widthFixedValues = List.map (\x -> round (x / sizePerChar)) relativeValues
        starRepresentation = List.map (\x -> x) widthFixedValues

    in starRepresentation
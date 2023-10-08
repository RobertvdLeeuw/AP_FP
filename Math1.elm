module Math1 exposing (..)

type Function
    = Const Float
    | X
    | Plus Function Function
    | Minus Function Function
    | Mult Function Function
    | Poly Function Int
    | Divide Function Function

print : Function -> String
print func =
    case func of
        Const n ->
            String.fromFloat n

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

        Divide numerator demoninator ->
            "(" ++ print numerator ++ " / " ++ print demoninator ++ ")"


eval: Function -> Float ->  Float
eval func x =
    case func of
        Const c ->
            c

        X ->
            x

        Plus f1 f2 ->
            eval f1 x + eval f2 x

        Minus f1 f2 ->
            eval f1 x - eval f2 x

        Mult f1 f2 ->
            eval f1 x * eval f2 x

        Poly f n ->
            (eval f x) ^ toFloat n -- Use ^ for exponentiation and Float.toFloat for type conversion

        Divide f1 f2 ->
            (eval f1 x) / eval f2 x


rangeWithSteps: Float -> Float -> Float -> List Float
rangeWithSteps current max step = if current > max then [] else current :: rangeWithSteps (current + step) max step


makeLine: Int -> Int -> Int -> String
makeLine max current amount =
    if current > max then "" else
        if current >= amount then String.concat ["-", makeLine max (current + 1) amount]
        else String.concat ["*", makeLine max (current + 1) amount]

graph: Function -> Int -> Int -> Int -> Int -> String
graph f xMin xMax yMin yMax =
    let width = 25
        length = 25

        sizePerChar = (toFloat yMax - toFloat yMin) / width
        sizePerLine = (toFloat xMax - toFloat xMin) / length
        xSteps = (rangeWithSteps (toFloat xMin) (toFloat xMax) sizePerLine)

        values = List.map (eval f) xSteps

        clampedValues = List.map (clamp (toFloat yMin) (toFloat yMax)) values
        relativeValues = List.map (\x -> x - (toFloat yMin)) clampedValues

        widthFixedValues = List.map (\x -> round (x / sizePerChar)) relativeValues

        lines = List.map(makeLine width 0) widthFixedValues
        withNewLines = List.map(\l -> String.concat [l, "\n"]) lines
    in String.concat withNewLines

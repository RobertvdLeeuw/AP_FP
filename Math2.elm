module Math2 exposing (..)

import Math1 exposing (..)


simplify: Function -> Function
simplify func =
    case func of
        Const x -> Const x

        X -> X

        Plus left right ->
            let solvedLeft = simplify left
                solvedRight = simplify right
            in case (solvedLeft, solvedRight) of
                (Const x, Const y) -> Const (x + y)

                (x, Const (toFloat 0)) -> x

                (Const (toFloat 0), x) -> x

                _ -> Plus solvedLeft solvedRight

        Minus left right ->
            let solvedLeft = simplify left
                solvedRight = simplify right
            in case (solvedLeft, solvedRight) of
                (Const x, Const y) -> Const (x - y)

                (x, Const (toFloat 0)) -> x

                (Const (toFloat 0), x) -> x

                _ -> Plus solvedLeft solvedRight

        Mult left right ->
            let solvedLeft = simplify left
                solvedRight = simplify right
            in case (solvedLeft, solvedRight) of
                (Const x, Const y) -> Const (x * y)

                (x, Const (toFloat 1)) -> x

                (Const (toFloat 1), x) ->  x

                _ -> Plus solvedLeft solvedRight

        Divide left right ->
            let solvedLeft = simplify left
                solvedRight = simplify right
            in case (solvedLeft, solvedRight) of
                (Const x, Const y) -> Const (x / y)

                (x, 1) -> x

                (1, x) ->  x

                _ -> Plus solvedLeft solvedRight

        Poly left right ->
            let solvedLeft = simplify left
            in case solvedLeft of
                Const x -> Const (x ^ (toFloat right))

                _ -> Poly solvedLeft right


-- Function to calculate the derivative
derivative : Function -> Function
derivative func =
    case func of
        Const _ ->
            Const 0

        X ->
            Const 1

        Plus left right ->
            Plus (derivative left) (derivative right)

        Minus left right ->
            Minus (derivative left) (derivative right)

        Divide left right ->
            let
                numeratorPart = Minus (Mult (derivative left) right) (Mult left (derivative right))
                denominatorPart = Mult right right
            in
            Divide numeratorPart denominatorPart

        Mult left right ->
            case (left, right) of
                -- Handle Const * X separately to preserve the constant
                (Const c, X) ->
                    Const c

                (X, Const c) ->
                    Const c

                (Const c, Poly X exp) ->
                    Mult (Const c) (derivative (Poly X exp))

                (Poly X exp, Const c) ->
                    Mult (Const c) (derivative (Poly X exp))

                _ ->
                    Plus (Mult (derivative left) right) (Mult left (derivative right))

        Poly base exp ->
            case base of
                X ->
                    Mult (Const (toFloat exp)) (Mult (derivative base) (Poly base (exp - 1)))

                _ -> Const 0

-- Example usage:
exampleFunc : Function
exampleFunc =
    Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)

-- Calculate the derivative of exampleFunc
derivativeResult : Function
derivativeResult =
    derivative exampleFunc

-- You can use derivativeResult as needed
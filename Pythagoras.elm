
module Pythagoras exposing (..)

sqr : Int -> Int
sqr number = number * number

isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
    if a < 0 || b < 0 || c < 0 then False
    else (sqr a + sqr b) == sqr c

leg1: Int -> Int -> Int
leg1 x y = sqr x - sqr y 

leg2: Int -> Int -> Int
leg2 x y = 2 * x * y 

hyp: Int -> Int -> Int
hyp x y = sqr x + sqr y

pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) = 
    if x >= 0 && y >= 0 && x>y then 
    (leg1 x y,leg2 x y, hyp x y) else
    (0, 0, 0)

isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) = isTriple a b c
    
pythTriplesMap: List (Int, Int) -> List (Int, Int, Int) 
pythTriplesMap inputList = List.map pythTriple inputList    

pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec inputList =
    case inputList of
        [] -> []
        x :: xs -> [pythTriple x] ++ pythTriplesRec xs

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter toCheck = List.filter isTripleTuple toCheck

arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec toCheck =
    case toCheck of
        [] -> []
        x :: xs -> (if isTripleTuple x then [x] else []) ++ arePythTriplesRec xs
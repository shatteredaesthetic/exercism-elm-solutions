module DifferenceOfSquares exposing (..)


square : Int -> Int
square n =
    n ^ 2


squareOfSum : Int -> Int
squareOfSum num =
    [1..num]
        |> List.foldl (+) 0
        |> square


sumOfSquares : Int -> Int
sumOfSquares num =
    [1..num]
        |> List.map square
        |> List.foldl (+) 0


difference : Int -> Int
difference num =
    squareOfSum num - sumOfSquares num

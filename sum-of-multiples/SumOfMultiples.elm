module SumOfMultiples exposing (..)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples mults limit =
    let
        isFactor y x =
            x % y == 0

        mults' =
            List.map isFactor mults

        areMults a =
            mults'
                |> List.map (\fn -> fn a)
                |> List.any identity
    in
        [2..(limit - 1)]
            |> List.filter areMults
            |> List.foldl (+) 0

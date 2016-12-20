module Hamming exposing (..)

import List.Extra as Extra
import String


distance : String -> String -> Maybe Int
distance dna1 dna2 =
    if String.length dna1 /= String.length dna2 then
        Nothing
    else
        let
            codes =
                Extra.zip (String.toList dna1) (String.toList dna2)
        in
            codes
                |> List.map calcDiff
                |> List.foldl (+) 0
                |> Just


calcDiff : ( Char, Char ) -> Int
calcDiff tup =
    case fst tup == snd tup of
        False ->
            1

        True ->
            0

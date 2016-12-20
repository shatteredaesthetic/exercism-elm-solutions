module Raindrops exposing (..)

import String


factors : Int -> List Int
factors n =
    let
        num =
            n // 2

        fappend =
            flip List.append
    in
        [2..num]
            |> fappend [ n ]
            |> List.filter (\x -> n % x == 0)


convertFactor : Int -> String -> ( String, List Int ) -> ( String, List Int )
convertFactor n str tup =
    let
        lst =
            snd tup

        s =
            if List.member n lst then
                str
            else
                ""

        newS =
            (fst tup) ++ s
    in
        ( newS, lst )


raindrops : Int -> String
raindrops num =
    let
        ans =
            ( "", factors num )
                |> convertFactor 3 "Pling"
                |> convertFactor 5 "Plang"
                |> convertFactor 7 "Plong"

        str =
            fst ans
    in
        if String.isEmpty str then
            toString num
        else
            str

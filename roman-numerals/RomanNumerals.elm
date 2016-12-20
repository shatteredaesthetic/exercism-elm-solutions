module RomanNumerals exposing (toRoman)


toRoman : Int -> String
toRoman num =
    ( num, "" )
        |> roman 1000 "M"
        |> roman 900 "CM"
        |> roman 500 "D"
        |> roman 400 "CD"
        |> roman 100 "C"
        |> roman 90 "XC"
        |> roman 50 "L"
        |> roman 40 "XL"
        |> roman 10 "X"
        |> roman 9 "IX"
        |> roman 5 "V"
        |> roman 4 "IV"
        |> roman 1 "I"
        |> snd


roman : Int -> String -> ( Int, String ) -> ( Int, String )
roman mx ch ( num, str ) =
    case compare num mx of
        LT ->
            ( num, str )

        EQ ->
            ( 0, str ++ ch )

        GT ->
            roman mx ch ( num - mx, str ++ ch )

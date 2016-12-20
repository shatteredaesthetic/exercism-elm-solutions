module LargestSeriesProduct exposing (largestProduct)

import List.Extra as Extra
import String
import Result
import Char


fGroups : Int -> List a -> List (List a)
fGroups num lst =
    Extra.groupsOfWithStep num 1 lst


largestProduct : Int -> String -> Maybe Int
largestProduct num str =
    let
        f =
            Result.withDefault 0 << String.toInt

        allNums =
            str
                |> String.toList
                |> List.all Char.isDigit
    in
        case compare num 0 of
            LT ->
                Nothing

            EQ ->
                Just 1

            GT ->
                if String.length str == 0 || String.length str < num || not allNums then
                    Nothing
                else
                    str
                        |> String.split ""
                        |> List.map f
                        |> fGroups num
                        |> List.map List.product
                        |> List.sort
                        |> Extra.last
                        |> Maybe.withDefault 0
                        |> Just

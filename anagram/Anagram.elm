module Anagram exposing (detect)

import Char
import String


detect : String -> List String -> List String
detect root possibles =
    possibles
        |> List.map (checkAnagram root)
        |> List.filter (\wrd -> wrd /= "")


checkAnagram : String -> String -> String
checkAnagram root poss =
    case sortString root == sortString poss of
        True ->
            if (String.toLower poss) == (String.toLower root) then
                ""
            else
                poss

        False ->
            ""


sortString : String -> String
sortString str =
    str
        |> String.toLower
        |> String.toList
        |> List.sortBy Char.toCode
        |> String.fromList

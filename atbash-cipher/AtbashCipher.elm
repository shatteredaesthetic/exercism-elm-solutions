module AtbashCipher exposing (..)

import List.Extra as Extra
import String
import Char exposing (fromCode, toCode)
import Regex exposing (regex, HowMany(..))


encode : String -> String
encode str =
    let
        encLst =
            List.map Char.fromCode [97..122]

        makeWords s =
            s
                |> String.toList
                |> Extra.greedyGroupsOf 5
                |> List.map String.fromList
                |> List.intersperse " "
                |> String.concat
    in
        str
            |> String.toLower
            |> Regex.replace All (regex "[^a-z0-9]") (\_ -> "")
            |> String.map (convert encLst)
            |> makeWords


decode : String -> String
decode str =
    let
        decLst =
            List.reverse <| List.map Char.fromCode [97..122]
    in
        str
            |> String.words
            |> String.join ""
            |> String.map (convert decLst)


convert : List Char -> Char -> Char
convert lst ch =
    let
        a2z =
            List.map Char.fromCode [97..122]
    in
        if Char.isDigit ch then
            ch
        else
            case Extra.elemIndex ch a2z of
                Nothing ->
                    'X'

                Just i ->
                    case Extra.getAt i <| List.reverse a2z of
                        Nothing ->
                            '#'

                        Just ch' ->
                            ch'

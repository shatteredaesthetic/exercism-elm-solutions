module RunLengthEncoding exposing (..)

import String
import List.Extra as Extra
import Regex exposing (HowMany(..), regex)
import ParseInt exposing (parseInt)


version : Int
version =
    2


encode : String -> String
encode str =
    str
        |> String.split ""
        |> Extra.group
        |> List.map encodeMap
        |> String.join ""


decode : String -> String
decode str =
    str
        |> Regex.find All (regex "[0-9]+[A-Za-z\\s]|[A-Za-z\\s]|[0-9]+[\\u2000-\\u3000]|[\\u2000-\\u3000]")
        |> List.map .match
        |> List.map decodeMap
        |> String.join ""


decodeMap : String -> String
decodeMap str =
    let
        pair =
            str
                |> Regex.find All (regex "[0-9]+|[A-Za-z\\s]|[\\u2000-\\u3000]")
                |> List.map .match

        num =
            pair
                |> List.head
                |> Maybe.withDefault ""
                |> parseInt
                |> Result.withDefault 0

        ch =
            pair
                |> Extra.last
                |> Maybe.withDefault ""
    in
        if List.length pair == 2 then
            String.repeat num ch
        else
            ch


encodeMap : List String -> String
encodeMap lst =
    let
        num =
            toString <| List.length lst

        str =
            Maybe.withDefault "" <| List.head lst
    in
        case num of
            "1" ->
                str

            _ ->
                num ++ str

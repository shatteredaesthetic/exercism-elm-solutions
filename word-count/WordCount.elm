module WordCount exposing (..)

import Dict exposing (Dict)
import String
import Char
import Regex exposing (HowMany(..), regex)
import List.Extra as Extra


wordCount : String -> Dict String Int
wordCount str =
    let
        fsort =
            Char.toCode << strToChar << String.left 1
    in
        str
            |> Regex.replace All (regex "[^A-Za-z0-9\\s+]+") (\_ -> "")
            |> String.toLower
            |> String.words
            |> List.sortBy fsort
            |> Extra.group
            |> List.map makeEntry
            |> Dict.fromList


makeEntry : List String -> ( String, Int )
makeEntry lst =
    let
        len =
            List.length lst

        entry =
            lst
                |> List.head
                |> Maybe.withDefault ""
    in
        ( entry, len )


strToChar : String -> Char
strToChar strCh =
    strCh
        |> String.toList
        |> List.head
        |> Maybe.withDefault '/'

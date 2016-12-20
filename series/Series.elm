module Series exposing (..)

import String
import Result exposing (Result(..))
import Regex exposing (HowMany(..), regex)
import Result.Extra as RExtra
import List.Extra as Extra
import ParseInt exposing (parseInt)


slices : Int -> String -> Result String (List (List Int))
slices size str =
    if size < 1 then
        Err ("Invalid size: " ++ toString size)
    else if Regex.contains (regex "[A-Za-z]") str then
        let
            ch =
                Regex.find (AtMost 1) (regex "[A-Za-z]") str
                    |> List.map .match
                    |> List.head
                    |> Maybe.withDefault ""
        in
            Err ("could not convert string '" ++ ch ++ "' to an Int")
    else
        str
            |> String.split ""
            |> List.map parseInt
            |> Extra.groupsOfWithStep size 1
            |> List.map RExtra.combine
            |> RExtra.combine
            |> Result.formatError (\_ -> "Bad Number")

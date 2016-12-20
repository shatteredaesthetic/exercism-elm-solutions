module PhoneNumber exposing (..)

import String
import Regex exposing (HowMany(..), regex)


prettyPrint : String -> Maybe String
prettyPrint str =
    case getNumber str of
        Nothing ->
            Nothing

        Just x ->
            Just <| parseNum ( "", x )


parseNum : ( String, String ) -> String
parseNum tup =
    case String.length <| snd tup of
        0 ->
            fst tup

        4 ->
            fst tup ++ snd tup

        10 ->
            let
                first =
                    "(" ++ (String.left 3 <| snd tup) ++ ") "
            in
                parseNum ( first, String.dropLeft 3 <| snd tup )

        _ ->
            let
                first =
                    fst tup ++ (String.left 3 <| snd tup) ++ "-"
            in
                parseNum ( first, String.dropLeft 3 <| snd tup )


getNumber : String -> Maybe String
getNumber str =
    let
        numStr =
            Regex.replace All (regex "[^0-9]") (\_ -> "") str
    in
        case String.length numStr of
            10 ->
                Just numStr

            11 ->
                if String.left 1 numStr == "1" then
                    Just <| String.dropLeft 1 numStr
                else
                    Nothing

            _ ->
                Nothing

module Pangram exposing (..)

import Regex
import String
import Char
import Set


-- 1. filter out all non-letters
-- 2. lowercase everything
-- 3. map to toCode (keyCode)
-- 4. sort numbers
-- 5. compare to [97-122]


dropDups : List comparable -> List comparable
dropDups lst =
    Set.fromList lst |> Set.toList


isPangram : String -> Bool
isPangram str =
    let
        ans =
            str
                |> Regex.replace Regex.All (Regex.regex "[^A-Za-z]") (\_ -> "")
                |> String.map Char.toLower
                |> String.toList
                |> List.map Char.toCode
                |> List.sort
                |> dropDups
    in
        ans == [97..122]

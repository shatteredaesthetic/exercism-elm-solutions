module Bob exposing (..)

import Regex
import String
import Char


type Statement
    = Question
    | Yelling
    | Silence
    | Other


hey : String -> String
hey statement =
    case statementType statement of
        Question ->
            "Sure."

        Yelling ->
            "Whoa, chill out!"

        Silence ->
            "Fine. Be that way!"

        Other ->
            "Whatever."


isQuestion : String -> Bool
isQuestion str =
    String.right 1 str == "?"


isAllCaps : String -> Bool
isAllCaps str =
    let
        ans =
            Regex.replace Regex.All (Regex.regex "[^A-Za-z]") (\_ -> "") str
    in
        case String.isEmpty ans of
            True ->
                False

            False ->
                String.all Char.isUpper ans


isStatementEmpty : String -> Bool
isStatementEmpty str =
    str
        |> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "")
        |> String.isEmpty


statementType : String -> Statement
statementType statement =
    if isQuestion statement && (not <| isAllCaps statement) then
        Question
    else if isAllCaps statement then
        Yelling
    else if String.isEmpty statement || isStatementEmpty statement then
        Silence
    else
        Other

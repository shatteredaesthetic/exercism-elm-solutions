module RNATranscription exposing (..)

import Result exposing (Result(..))
import String
import Array


toRNA : String -> Result Char String
toRNA dna =
    let
        invalid =
            String.filter (not << inDNA) dna
    in
        if String.isEmpty invalid then
            dna
                |> String.map switchBases
                |> Ok
        else
            invalid
                |> getCharAt 0
                |> Maybe.withDefault 'X'
                |> Err


switchBases : Char -> Char
switchBases ch =
    case ch of
        'A' ->
            'U'

        'T' ->
            'A'

        'C' ->
            'G'

        'G' ->
            'C'

        _ ->
            ch


inDNA : Char -> Bool
inDNA ch =
    List.member ch [ 'A', 'C', 'G', 'T' ]


inRNA : Char -> Bool
inRNA ch =
    List.member ch [ 'U', 'C', 'G', 'T' ]


getCharAt : Int -> String -> Maybe Char
getCharAt i str =
    str
        |> String.toList
        |> Array.fromList
        |> Array.get i

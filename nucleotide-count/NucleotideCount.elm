module NucleotideCount exposing (..)

import String
import List.Extra as Extra


version : Int
version =
    2


type alias Nucleotides =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> Nucleotides
nucleotideCounts strand =
    let
        counts =
            Nucleotides 0 0 0 0
    in
        strand
            |> String.toList
            |> List.map (countNucs counts)
            |> Extra.foldl1 reduceRecord
            |> Maybe.withDefault counts


countNucs : Nucleotides -> Char -> Nucleotides
countNucs cnt ch =
    case ch of
        'A' ->
            { cnt | a = cnt.a + 1 }

        'T' ->
            { cnt | t = cnt.t + 1 }

        'C' ->
            { cnt | c = cnt.c + 1 }

        'G' ->
            { cnt | g = cnt.g + 1 }

        _ ->
            cnt


reduceRecord : Nucleotides -> Nucleotides -> Nucleotides
reduceRecord x y =
    { a = x.a + y.a
    , t = x.t + y.t
    , c = x.c + y.c
    , g = x.g + y.g
    }

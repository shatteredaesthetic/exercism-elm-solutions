module Strain exposing (..)


keep : (a -> Bool) -> List a -> List a
keep pred list =
    case list of
        [] ->
            []

        x :: xs ->
            if pred x then
                x :: keep pred xs
            else
                keep pred xs


discard : (a -> Bool) -> List a -> List a
discard f lst =
    keep (not << f) lst

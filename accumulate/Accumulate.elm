module Accumulate exposing (..)


accumulate : (a -> b) -> List a -> List b
accumulate f l =
    case l of
        [] ->
            []

        x :: xs ->
            f x :: accumulate f xs

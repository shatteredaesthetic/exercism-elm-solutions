module Sublist exposing (..)


version : Int
version =
    2


type ListComparison
    = Equal
    | Sublist
    | Superlist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist a b =
    let
        nonEmpty x y =
            case compare (List.length x) (List.length y) of
                EQ ->
                    if x == y then
                        Equal
                    else
                        Unequal

                LT ->
                    if isASublist x y then
                        Sublist
                    else
                        Unequal

                GT ->
                    if isASublist y x then
                        Superlist
                    else
                        Unequal
    in
        case ( a, b ) of
            ( [], [] ) ->
                Equal

            ( [], _ ) ->
                Sublist

            ( _, [] ) ->
                Superlist

            ( _, _ ) ->
                nonEmpty a b


isASublist : List a -> List a -> Bool
isASublist poss list =
    let
        len =
            List.length poss

        sub =
            List.take len list
    in
        case list of
            [] ->
                False

            x :: xs ->
                if poss == sub then
                    True
                else
                    isASublist poss <| List.drop 1 list

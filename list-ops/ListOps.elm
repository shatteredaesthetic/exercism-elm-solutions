module ListOps exposing (..)


length : List a -> Int
length list =
    case list of
        [] ->
            0

        x :: xs ->
            1 + length xs


foldl : (a -> b -> b) -> b -> List a -> b
foldl f x list =
    case list of
        [] ->
            x

        y :: ys ->
            foldl f (f y x) ys


reverse : List a -> List a
reverse list =
    foldl (::) [] list


foldr : (a -> b -> b) -> b -> List a -> b
foldr f x list =
    foldl f x <| reverse list


map : (a -> b) -> List a -> List b
map f list =
    case list of
        [] ->
            []

        x :: xs ->
            f x :: map f xs


filter : (a -> Bool) -> List a -> List a
filter pred list =
    case list of
        [] ->
            []

        x :: xs ->
            if pred x then
                x :: filter pred xs
            else
                filter pred xs


append : List a -> List a -> List a
append xs ys =
    case ys of
        [] ->
            xs

        _ ->
            ys
                |> foldl (::) (reverse xs)
                |> reverse


concat : List (List a) -> List a
concat list =
    case list of
        [] ->
            []

        x :: xs ->
            foldr append [] list

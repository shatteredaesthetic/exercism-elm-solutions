module Triangle exposing (..)


version : Int
version =
    2


type Triangle
    = Isosceles
    | Scalene
    | Equilateral


triangleKind : Float -> Float -> Float -> Result String Triangle
triangleKind x y z =
    let
        hypo =
            [ x, y, z ]
                |> List.maximum
                |> Maybe.withDefault 0

        isValid =
            if hypo == x then
                y + z >= hypo
            else if hypo == y then
                x + z >= hypo
            else
                x + y >= hypo

        sameSides =
            x == y && y == z

        diffSides =
            x /= y && y /= z && x /= z
    in
        if List.any (\n -> n <= 0) [ x, y, z ] then
            Err "Invalid lengths"
        else if not isValid then
            Err "Violates inequality"
        else if sameSides then
            Ok Equilateral
        else if diffSides && isValid then
            Ok Scalene
        else
            Ok Isosceles

module SpaceAge exposing (..)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


ageOn : Planet -> Int -> Float
ageOn planet seconds =
    let
        earth =
            toFloat 31557600

        secs =
            toFloat seconds
    in
        case planet of
            Mercury ->
                secs / (0.2408467 * earth)

            Venus ->
                secs / (0.61519726 * earth)

            Earth ->
                secs / earth

            Mars ->
                secs / (1.8808158 * earth)

            Jupiter ->
                secs / (11.862615 * earth)

            Saturn ->
                secs / (29.447498 * earth)

            Uranus ->
                secs / (84.016846 * earth)

            Neptune ->
                secs / (164.79132 * earth)

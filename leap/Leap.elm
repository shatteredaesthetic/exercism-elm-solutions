module Leap exposing (..)


type alias Year =
    Int


isLeapYear : Year -> Bool
isLeapYear year =
    (year % 400 == 0 || year % 100 /= 0) && (year % 4 == 0)

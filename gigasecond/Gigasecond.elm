module Gigasecond exposing (add)

import Date exposing (Date)


add : Date -> Date
add date =
    date
        |> Date.toTime
        |> (+) 1000000000000
        |> Date.fromTime

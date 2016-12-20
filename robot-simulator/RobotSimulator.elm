module RobotSimulator exposing (..)

import List.Extra as Extra
import String


type Bearing
    = North
    | West
    | South
    | East


type alias Point =
    { x : Int
    , y : Int
    }


type alias Robot =
    { bearing : Bearing
    , coordinates : Point
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = Point 0 0
    }


turnRight : Robot -> Robot
turnRight robot =
    let
        bearing' =
            case robot.bearing of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North
    in
        { robot | bearing = bearing' }


turnLeft : Robot -> Robot
turnLeft robot =
    let
        bearing' =
            case robot.bearing of
                North ->
                    West

                East ->
                    North

                South ->
                    East

                West ->
                    South
    in
        { robot | bearing = bearing' }


advance : Robot -> Robot
advance robot =
    { robot | coordinates = updateCoords robot.coordinates robot.bearing }


updateCoords : Point -> Bearing -> Point
updateCoords point direction =
    case direction of
        North ->
            { point | y = point.y + 1 }

        South ->
            { point | y = point.y - 1 }

        East ->
            { point | x = point.x + 1 }

        West ->
            { point | x = point.x - 1 }


simulate : String -> Robot -> Robot
simulate moves robot =
    let
        f lst =
            case lst of
                [] ->
                    identity

                fn :: [] ->
                    fn

                fn :: fns ->
                    fn >> f fns

        chooseOp ch =
            case ch of
                'L' ->
                    turnLeft

                'R' ->
                    turnRight

                'A' ->
                    advance

                _ ->
                    identity

        pipe =
            moves
                |> String.toList
                |> List.map chooseOp
    in
        robot
            |> f pipe

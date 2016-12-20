module GradeSchool exposing (addStudent, studentsInGrade, allStudents, empty)

import Dict exposing (Dict)
import String
import Char


type alias Grade =
    ( Int, List String )


type alias School =
    Dict Int (List String)


empty : School
empty =
    Dict.empty


addStudent : Int -> String -> School -> School
addStudent grade name school =
    case Dict.get grade school of
        Nothing ->
            Dict.insert grade [ name ] school

        Just _ ->
            Dict.update grade (update' name) school


studentsInGrade : Int -> School -> List String
studentsInGrade grade school =
    school
        |> Dict.get grade
        |> Maybe.withDefault []


allStudents : School -> List Grade
allStudents school =
    Dict.toList school


update' : String -> Maybe (List String) -> Maybe (List String)
update' name mVal =
    case mVal of
        Nothing ->
            Just [ name ]

        Just val ->
            val
                |> (::) name
                |> List.sort
                |> Just

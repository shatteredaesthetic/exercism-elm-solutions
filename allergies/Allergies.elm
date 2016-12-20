module Allergies exposing (isAllergicTo, toList)


type alias Entry =
    { label : String
    , score : Int
    }


allergies : List Entry
allergies =
    [ Entry "cats" 128
    , Entry "pollen" 64
    , Entry "chocolate" 32
    , Entry "tomatoes" 16
    , Entry "strawberries" 8
    , Entry "shellfish" 4
    , Entry "peanuts" 2
    , Entry "eggs" 1
    ]


isAllergicTo : String -> Int -> Bool
isAllergicTo label score =
    toList score
        |> List.member label


toList : Int -> List String
toList score =
    if score > 256 then
        toList <| score - 256
    else
        allergies
            |> List.foldl reduceAllergies ( [], score )
            |> fst


reduceAllergies : Entry -> ( List String, Int ) -> ( List String, Int )
reduceAllergies entry result =
    let
        lst =
            fst result

        remain =
            snd result
    in
        case compare remain entry.score of
            LT ->
                ( lst, remain )

            EQ ->
                ( entry.label :: lst, 0 )

            GT ->
                ( entry.label :: lst, remain - entry.score )

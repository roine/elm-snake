module Leaderboard exposing (UserScore, view)

import Html exposing (li, text, ul)


type alias UserScore =
    { name : String, score : Int }


view score =
    let
        sortLeaderboard =
            List.sortBy .score
    in
    ul []
        (List.map
            (\user ->
                li [] [ text (user.name ++ " " ++ String.fromInt user.score) ]
            )
            (sortLeaderboard score)
        )

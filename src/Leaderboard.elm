module Leaderboard exposing (Leaderboard, UserScore, view)

import Html exposing (Html, li, text, ul)
import Html.Attributes exposing (style)


type alias UserScore =
    { name : String, score : Int, uuid : String }


type alias Leaderboard =
    List UserScore


view : Leaderboard -> UserScore -> Html msg
view score current =
    let
        sortLeaderboard =
            List.sortBy .score

        highlightStyle =
            [ style "background" " green", style "color" "white" ]
    in
    ul []
        (List.map
            (\user ->
                li
                    (if user.uuid == current.uuid then
                        highlightStyle

                     else
                        []
                    )
                    [ text (user.name ++ " " ++ String.fromInt user.score) ]
            )
            (sortLeaderboard score |> List.reverse)
        )

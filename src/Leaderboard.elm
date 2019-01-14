module Leaderboard exposing (Leaderboard, UserScore, view, resetScore)

import Dict exposing (Dict)
import Html exposing (Html, li, text, ul)
import Html.Attributes exposing (class, style)


type alias UserScore =
    { name : String, score : Int }


type alias Leaderboard =
    Dict String UserScore


resetScore : String -> Leaderboard -> Leaderboard
resetScore currentUserId leaderboard =
    Dict.update currentUserId
        (\maybeUser ->
            case maybeUser of
                Nothing -> Nothing
                Just u -> Just {u|score = 0})
            leaderboard



view : Leaderboard -> String -> Html msg
view leaderboard currentUserId =
    let
        sortLeaderboard =
            List.sortBy .score

        highlightStyle =
            [ style "background" " green", style "color" "white" ]

        currentUser =
            Dict.get currentUserId leaderboard
    in
    ul [ class "no-bullet" ]
        (List.map
            (\( key, user ) ->
                li
                    (if key == currentUserId then
                        highlightStyle

                     else
                        []
                    )
                    [ text (user.name ++ " " ++ String.fromInt user.score) ]
            )
            (Dict.toList leaderboard |> List.sortBy (.score << Tuple.second) |> List.reverse)
        )

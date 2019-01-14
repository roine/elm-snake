module GameStates.Over exposing (Model, Msg(..), update, view)

import Dict
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Leaderboard exposing (Leaderboard, UserScore)



-- MODEL


type alias Model =
    { currentUserId : String
    , leaderboard : Leaderboard
    }



-- UPDATE


type Msg
    = ChangeState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { currentUserId, leaderboard } =
    let
        currentUser =
            Dict.get currentUserId leaderboard
    in
    div []
        [ div [] [ text "Game Over" ]
        , button [ onClick ChangeState, class "button" ] [ text "restart" ]
        , case currentUser of
            Nothing ->
                text ""

            Just u ->
                text (u.name ++ "'s score: " ++ String.fromInt u.score)
        ]

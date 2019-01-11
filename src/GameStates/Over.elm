module GameStates.Over exposing (Model, Msg(..), update, view)

import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Leaderboard exposing (UserScore)



-- MODEL


type alias Model =
    { score : Int
    , name : String
    , leaderboard : List UserScore
    }



-- UPDATE


type Msg
    = ChangeState
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view { name, score } =
    div []
        [ text "Game Over"
        , button [ onClick ChangeState ] [ text "restart" ]
        , text (name ++ "'s score: " ++ String.fromInt score)
        ]
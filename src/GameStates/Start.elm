module GameStates.Start exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Leaderboard exposing (UserScore)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Uuid exposing (Uuid)



-- MODEL


init ( seed, seedExtension ) =
    let
        ( newUuid, newSeed ) =
            step Uuid.generator (initialSeed seed seedExtension)
    in
    { name = ""
    , leaderboard = []
    , currentSeed = newSeed
    , currentUUID = newUuid
    }


type alias Model =
    { name : String
    , leaderboard : List UserScore
    , currentSeed : Seed
    , currentUUID : Uuid
    }



-- UPDATE


type Msg
    = UpdateName String
    | ChangeState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName newName ->
            ( { model | name = newName }, Cmd.none )

        ChangeState ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.name, onInput UpdateName ] []
        , button [ disabled (String.isEmpty model.name), onClick ChangeState ] [ text "Start" ]
        , text (Debug.toString model)
        ]

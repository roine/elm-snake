module GameStates.Start exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, type_, value)
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
    , uuid = Uuid.toString newUuid
    , leaderboard = []
    , currentSeed = newSeed
    , currentUUID = newUuid
    }


type alias Model =
    { name : String
    , uuid : String
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
            ( { model
                | name = newName
              }
            , Cmd.none
            )

        ChangeState ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "input-group" ]
        [ input
            [ class "input-group-field"
            , value model.name
            , onInput UpdateName
            , type_ "text"
            ]
            []
        , div [ class "input-group-button" ]
            [ button
                [ disabled (String.isEmpty model.name)
                , onClick ChangeState
                , class "button"
                ]
                [ text "Start" ]
            ]
        ]

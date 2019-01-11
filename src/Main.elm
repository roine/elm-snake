port module Main exposing (main)

import Browser
import GameStates exposing (GameStatus(..), getUserScoreFromModel)
import GameStates.Over
import GameStates.Play
import GameStates.Start
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Leaderboard exposing (UserScore)


port leaderboard : (List UserScore -> msg) -> Sub msg


type alias Model =
    GameStatus


type alias Flags =
    ( Int, List Int )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( StartPageState (GameStates.Start.init flags)
    , Cmd.none
    )



-- UPDATE


type Msg
    = PlayingType GameStates.Play.Msg
    | GameOverType GameStates.Over.Msg
    | StartPageType GameStates.Start.Msg
    | AddLeaderboard (List { name : String, score : Int })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- TRANSITION GAME STATE
        ( StartPageType GameStates.Start.ChangeState, StartPageState startState ) ->
            ( PlayingState (GameStates.Play.init startState.name), Cmd.none )

        ( PlayingType GameStates.Play.ChangeState, PlayingState playingState ) ->
            ( GameOverState { name = playingState.name, score = playingState.score, leaderboard = playingState.leaderboard }, Cmd.none )

        ( GameOverType GameStates.Over.ChangeState, GameOverState overState ) ->
            ( PlayingState (GameStates.Play.init overState.name), Cmd.none )

        -- FORWARDING MSG
        ( StartPageType subMsg, StartPageState startState ) ->
            GameStates.Start.update subMsg startState
                |> Tuple.mapBoth StartPageState (Cmd.map StartPageType)

        ( PlayingType subMsg, PlayingState playingState ) ->
            GameStates.Play.update subMsg playingState
                |> Tuple.mapBoth PlayingState (Cmd.map PlayingType)

        ( GameOverType subMsg, GameOverState gameOverState ) ->
            GameStates.Over.update subMsg gameOverState
                |> Tuple.mapBoth GameOverState (Cmd.map GameOverType)

        ( AddLeaderboard scores, PlayingState playingState ) ->
            ( PlayingState { playingState | leaderboard = scores }, Cmd.none )

        ( AddLeaderboard scores, StartPageState startState ) ->
            ( StartPageState { startState | leaderboard = scores }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ div []
            [ case model of
                StartPageState startPageModel ->
                    Html.map StartPageType (GameStates.Start.view startPageModel)

                PlayingState playingModel ->
                    Html.map PlayingType (GameStates.Play.view playingModel)

                GameOverState gameOverModel ->
                    Html.map GameOverType (GameStates.Over.view gameOverModel)
            ]
        , Leaderboard.view (getUserScoreFromModel model)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ leaderboard AddLeaderboard
        , case model of
            PlayingState playingState ->
                Sub.map PlayingType (GameStates.Play.subscriptions playingState)

            _ ->
                Sub.none
        ]



-- INIT


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

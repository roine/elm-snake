port module Main exposing (main)

import Browser
import Dict
import GameStates exposing (GameStatus(..), getLeaderboardFromModel, getUserIdFromModel)
import GameStates.Over
import GameStates.Play
import GameStates.Start
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode
import Leaderboard exposing (UserScore)
import Uuid


port leaderboard : (List ( String, UserScore ) -> msg) -> Sub msg


port sendScore : ( String, UserScore ) -> Cmd msg


port sentScore : (( String, UserScore ) -> msg) -> Sub msg



--port scoreSent :


type alias Model =
    GameStatus


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( StartPageState GameStates.Start.init
    , Cmd.none
    )



-- UPDATE


type Msg
    = PlayingType GameStates.Play.Msg
    | GameOverType GameStates.Over.Msg
    | StartPageType GameStates.Start.Msg
    | AddLeaderboard (List ( String, UserScore ))
    | AddIdToUser ( String, UserScore )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        -- TRANSITION GAME STATE
        ( StartPageType GameStates.Start.ChangeState, StartPageState startState ) ->
            ( StartPageState startState, sendScore ( "", { name = startState.name, score = 0 } ) )

        ( PlayingType GameStates.Play.ChangeState, PlayingState playingState ) ->
            let
                currentUser =
                    Dict.get playingState.currentUserId playingState.leaderboard
            in
            ( GameOverState
                { leaderboard = playingState.leaderboard
                , currentUserId = playingState.currentUserId
                }
            , case currentUser of
                Nothing ->
                    Cmd.none

                Just u ->
                    sendScore ( playingState.currentUserId, { name = u.name, score = u.score } )
            )

        ( GameOverType GameStates.Over.ChangeState, GameOverState overState ) ->
            ( PlayingState (GameStates.Play.init overState.currentUserId overState.leaderboard), Cmd.none )

        -- FORWARDING MSG
        ( StartPageType subMsg, StartPageState startState ) ->
            GameStates.Start.update subMsg startState
                |> Tuple.mapBoth StartPageState (Cmd.map StartPageType)

        ( PlayingType subMsg, PlayingState playingState ) ->
            GameStates.Play.update subMsg playingState
                |> Tuple.mapBoth PlayingState (Cmd.map PlayingType)

        --        ( GameOverType subMsg, GameOverState gameOverState ) ->
        --            GameStates.Over.update subMsg gameOverState
        --                |> Tuple.mapBoth GameOverState (Cmd.map GameOverType)
        ( AddLeaderboard l, PlayingState playingState ) ->
            ( PlayingState { playingState | leaderboard = Dict.fromList l }, Cmd.none )

        ( AddLeaderboard l, StartPageState startState ) ->
            ( StartPageState { startState | leaderboard = Dict.fromList l }, Cmd.none )

        ( AddLeaderboard l, GameOverState gameOverState ) ->
            ( GameOverState { gameOverState | leaderboard = Dict.fromList l }, Cmd.none )

        --                |> Tuple.mapBoth GameOverState (Cmd.map GameOverType)
        ( AddIdToUser ( remoteId, user ), StartPageState startState ) ->
            ( PlayingState
                (GameStates.Play.init
                    remoteId
                    (Dict.map
                        (\id u ->
                            if remoteId == id then
                                user

                            else
                                u
                        )
                        startState.leaderboard
                    )
                )
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        [ div
            [ class "grid-x align-center" ]
            [ div [ class "row medium-7 columns" ]
                [ case model of
                    StartPageState startPageModel ->
                        Html.map StartPageType (GameStates.Start.view startPageModel)

                    PlayingState playingModel ->
                        Html.map PlayingType (GameStates.Play.view playingModel)

                    GameOverState gameOverModel ->
                        Html.map GameOverType (GameStates.Over.view gameOverModel)
                , Leaderboard.view (getLeaderboardFromModel model) (getUserIdFromModel model)
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ leaderboard AddLeaderboard
        , sentScore AddIdToUser
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

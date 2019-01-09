module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Random
import Time exposing (Posix)


type Direction
    = West
    | North
    | East
    | South


oppositeDirection directionA directionB =
    case ( directionA, directionB ) of
        ( West, East ) ->
            True

        ( East, West ) ->
            True

        ( North, South ) ->
            True

        ( South, North ) ->
            True

        _ ->
            False


type alias Model =
    GameStatus


type GameStatus
    = PlayingState PlayingModel
    | StartPageState {}
    | GameOverState { score : Int }


type alias PlayingModel =
    { snakePosition : List ( Int, Int )
    , lastTick : Posix
    , lastPositionWhenDirectionChanged : ( Int, Int ) -- used to avoid more than one direction change per snake move
    , direction : Directions
    , difficulty : Int
    , fruitPosition : Maybe ( Int, Int )
    , score : Int
    , paused : Bool
    }


type alias Flags =
    ()



-- Allow to register the next move


type Directions
    = Directions Direction (Maybe Direction)


getNextDirection (Directions direction _) =
    direction


getLastDirection (Directions _ direction) =
    direction


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialPlayingState
    , Cmd.none
    )


initialPlayingState =
    PlayingState
        { snakePosition = [ ( 10, 4 ) ]
        , lastTick = Time.millisToPosix 0
        , lastPositionWhenDirectionChanged = ( 10, 4 )
        , direction = Directions East Nothing
        , difficulty = 1
        , fruitPosition = Nothing
        , score = 0
        , paused = False
        }


arenaDimension =
    ( 20, 20 )


blockDimension =
    15


difficulties : Dict Int Int
difficulties =
    Dict.fromList [ ( 1, 1000 ), ( 2, 500 ), ( 3, 300 ), ( 4, 200 ), ( 5, 100 ) ]



-- UPDATE


type Msg
    = PlayingType PlayingMsg
    | GameOverType GameOverMsg


type PlayingMsg
    = Tick Posix
    | ChangeDirection Direction
    | NewFruit ( Int, Int )
    | ChangeDifficulty String
    | StopPause


type GameOverMsg
    = Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PlayingType subMsg, PlayingState playingState ) ->
            updatePlaying subMsg playingState

        ( PlayingType subMsg, StartPageState startState ) ->
            updateStartPage subMsg startState

        ( GameOverType subMsg, GameOverState gameOverState ) ->
            updateGameOver subMsg gameOverState

        _ ->
            ( model, Cmd.none )


updatePlaying : PlayingMsg -> PlayingModel -> ( Model, Cmd Msg )
updatePlaying msg model =
    case msg of
        Tick posix ->
            if Time.posixToMillis posix - Time.posixToMillis model.lastTick > (Dict.get model.difficulty difficulties |> Maybe.withDefault 1000) then
                case model.snakePosition of
                    [] ->
                        ( PlayingState model, Cmd.none )

                    head :: [] ->
                        let
                            newPosition =
                                case model.direction of
                                    Directions West _ ->
                                        Tuple.mapFirst (\x -> x - 1) head

                                    Directions North _ ->
                                        Tuple.mapSecond (\y -> y - 1) head

                                    Directions East _ ->
                                        Tuple.mapFirst (\x -> x + 1) head

                                    Directions South _ ->
                                        Tuple.mapSecond (\y -> y + 1) head

                            newFruit =
                                case model.fruitPosition of
                                    Nothing ->
                                        getNewFruit

                                    Just fruitPosition ->
                                        if fruitPosition == head then
                                            getNewFruit

                                        else
                                            Cmd.none
                        in
                        if outOfBound newPosition then
                            ( GameOverState { score = model.score }, Cmd.none )

                        else
                            ( PlayingState
                                { model
                                    | snakePosition =
                                        case model.fruitPosition of
                                            Just fruitPosition ->
                                                if fruitPosition == head then
                                                    newPosition :: [ head ]

                                                else
                                                    [ newPosition ]

                                            Nothing ->
                                                [ newPosition ]
                                    , lastTick = posix
                                    , score =
                                        case model.fruitPosition of
                                            Just fruitPosition ->
                                                if fruitPosition == head then
                                                    model.score + 50

                                                else
                                                    model.score

                                            Nothing ->
                                                model.score
                                    , direction =
                                        case getLastDirection model.direction of
                                            Nothing ->
                                                model.direction

                                            Just lastDirection ->
                                                Directions lastDirection Nothing
                                    , fruitPosition =
                                        case model.fruitPosition of
                                            Just fruitPosition ->
                                                if fruitPosition == head then
                                                    Nothing

                                                else
                                                    model.fruitPosition

                                            Nothing ->
                                                model.fruitPosition
                                }
                            , newFruit
                            )

                    head :: tail ->
                        ( PlayingState model, Cmd.none )

            else
                ( PlayingState model, Cmd.none )

        ChangeDirection newDirection ->
            case model.snakePosition of
                headPosition :: tail ->
                    if model.lastPositionWhenDirectionChanged == headPosition then
                        case model.direction of
                            Directions oldDirection Nothing ->
                                ( PlayingState
                                    { model
                                        | direction = Directions (getNextDirection model.direction) (Just newDirection)
                                    }
                                , Cmd.none
                                )

                            _ ->
                                ( PlayingState model, Cmd.none )

                    else
                        ( PlayingState
                            { model
                                | direction = Directions newDirection Nothing
                                , lastPositionWhenDirectionChanged = headPosition
                            }
                        , Cmd.none
                        )

                [] ->
                    ( PlayingState model, Cmd.none )

        NewFruit newPosition ->
            ( PlayingState { model | fruitPosition = Just newPosition }, Cmd.none )

        ChangeDifficulty difficulty ->
            ( PlayingState { model | difficulty = Maybe.withDefault model.difficulty (String.toInt difficulty), paused = True }, Cmd.none )

        StopPause ->
            ( PlayingState { model | paused = False }, Cmd.none )


getNewFruit : Cmd Msg
getNewFruit =
    Random.generate (PlayingType << NewFruit) (Random.pair (Random.int 1 (Tuple.first arenaDimension)) (Random.int 1 (Tuple.second arenaDimension)))


updateStartPage msg model =
    ( StartPageState model, Cmd.none )


updateGameOver msg model =
    case msg of
        Restart ->
            ( initialPlayingState, Cmd.none )


outOfBound : ( Int, Int ) -> Bool
outOfBound position =
    Tuple.first position < 1 || Tuple.first position > Tuple.first arenaDimension || Tuple.second position < 1 || Tuple.second position > Tuple.second arenaDimension



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "display" "flex" ]
            [ case model of
                PlayingState playingModel ->
                    Html.map PlayingType (arenaView playingModel)

                StartPageState startPageModel ->
                    text "start page"

                GameOverState gameOverModel ->
                    div []
                        [ text "game over"
                        , button [ onClick (GameOverType Restart) ] [ text "restart" ]
                        ]
            ]
        , Debug.toString model |> text
        ]


arenaView : PlayingModel -> Html PlayingMsg
arenaView { snakePosition, fruitPosition, difficulty, paused } =
    let
        columns =
            Tuple.first arenaDimension

        rows =
            Tuple.second arenaDimension

        width =
            (blockDimension * rows |> String.fromInt) ++ "px"

        height =
            (blockDimension * columns |> String.fromInt) ++ "px"
    in
    div [ style "display" "flex" ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "height" height
            , style "width" width
            , style "border" "1px solid red"
            , style "position" "relative"
            ]
            (List.map
                (\y ->
                    div [ style "display" "flex", style "width" width, style "flex-grow" "1" ]
                        (List.map
                            (\x ->
                                let
                                    snakeStyle =
                                        case snakePosition of
                                            [] ->
                                                []

                                            head :: tail ->
                                                if x == Tuple.first head && y == Tuple.second head then
                                                    [ style "background" "red" ]

                                                else
                                                    []

                                    fruitStyle =
                                        case fruitPosition of
                                            Nothing ->
                                                []

                                            Just position ->
                                                if x == Tuple.first position && y == Tuple.second position then
                                                    [ style "background" "black" ]

                                                else
                                                    []
                                in
                                div ([ style "flex-grow" "1" ] ++ fruitStyle ++ snakeStyle)
                                    []
                            )
                            (List.range 1 rows)
                        )
                )
                (List.range 1 columns)
                ++ (if paused then
                        [ div
                            [ style "position" "absolute"
                            , style "left" "50%"
                            , style "top" "50%"
                            , style "transform" "translate(-50%, -50%)"
                            ]
                            [ button
                                [ onClick StopPause
                                , style "font-size" "40px"
                                , style "border" "0"
                                , style "padding" "0 15px"
                                ]
                                [ text "II" ]
                            ]
                        ]

                    else
                        []
                   )
            )
        , div []
            [ div [] [ text "Change difficulty" ]
            , div []
                [ input
                    [ type_ "range"
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "5"
                    , value (String.fromInt difficulty)
                    , onInput ChangeDifficulty
                    ]
                    []
                ]
            ]
        ]



-- SUBSCRIPTIONS


keyToDirections : Int -> Maybe Direction
keyToDirections key =
    case key of
        37 ->
            Just West

        38 ->
            Just North

        39 ->
            Just East

        40 ->
            Just South

        65 ->
            Just West

        87 ->
            Just North

        68 ->
            Just East

        83 ->
            Just South

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        PlayingState playingState ->
            if playingState.paused then
                Sub.none

            else
                Sub.batch
                    [ Browser.Events.onAnimationFrame (PlayingType << Tick)
                    , Browser.Events.onKeyPress
                        (Json.Decode.field "keyCode" Json.Decode.int
                            |> Json.Decode.map keyToDirections
                            |> Json.Decode.andThen
                                (\newDirection ->
                                    case newDirection of
                                        Nothing ->
                                            Json.Decode.fail "Wrong key used"

                                        Just direction ->
                                            if direction == getNextDirection playingState.direction || oppositeDirection direction (getNextDirection playingState.direction) then
                                                Json.Decode.fail "opposite direction or same direction"

                                            else
                                                Json.Decode.succeed direction
                                )
                            |> Json.Decode.map (PlayingType << ChangeDirection)
                        )
                    ]

        _ ->
            Sub.none



-- INIT


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

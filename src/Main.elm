module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..))
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
    | GameOverState GameOverModel


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


type alias GameOverModel =
    { score : Int
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
init _ =
    ( initialPlayingState
    , Cmd.none
    )


initialPlayingState =
    PlayingState
        { snakePosition = [ ( 10, 4 ), ( 9, 4 ) ]
        , lastTick = Time.millisToPosix 0
        , lastPositionWhenDirectionChanged = ( 10, 4 )
        , direction = Directions East Nothing
        , difficulty = 3
        , fruitPosition = Nothing
        , score = 0
        , paused = False
        }


arenaDimension =
    ( 15, 15 )


blockDimension =
    15


type alias Level =
    Int


type alias FrameRate =
    Int


type alias Score =
    Int


difficulties : Dict Level ( FrameRate, Score )
difficulties =
    Dict.fromList
        [ ( 1, ( 1000, 50 ) )
        , ( 2, ( 500, 100 ) )
        , ( 3, ( 300, 150 ) )
        , ( 4, ( 200, 300 ) )
        , ( 5, ( 100, 500 ) )
        ]


getSpeedFromDifficulty : Int -> Int
getSpeedFromDifficulty level =
    Dict.get level difficulties
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 1000


getScoreFromDifficulty : Int -> Int
getScoreFromDifficulty level =
    Dict.get level difficulties
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0



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
    | TogglePause Browser.Events.Visibility


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
            if Time.posixToMillis posix - Time.posixToMillis model.lastTick > getSpeedFromDifficulty model.difficulty then
                case model.snakePosition of
                    [] ->
                        ( PlayingState model, Cmd.none )

                    head :: tail ->
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
                        if outOfBound newPosition || ouroborosing head model.snakePosition then
                            ( GameOverState { score = model.score }, Cmd.none )

                        else
                            ( PlayingState
                                { model
                                    | snakePosition =
                                        case model.fruitPosition of
                                            Just fruitPosition ->
                                                if fruitPosition == head then
                                                    [ newPosition, head ] ++ tail

                                                else
                                                    case List.reverse model.snakePosition of
                                                        [] ->
                                                            [ newPosition ]

                                                        _ :: xs ->
                                                            newPosition :: List.reverse xs

                                            Nothing ->
                                                case List.reverse model.snakePosition of
                                                    [] ->
                                                        [ newPosition ]

                                                    _ :: xs ->
                                                        newPosition :: List.reverse xs
                                    , lastTick = posix
                                    , score =
                                        case model.fruitPosition of
                                            Just fruitPosition ->
                                                if fruitPosition == head then
                                                    model.score + getScoreFromDifficulty model.difficulty + 5

                                                else
                                                    model.score + 5

                                            Nothing ->
                                                model.score + 5
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

            else
                ( PlayingState model, Cmd.none )

        ChangeDirection newDirection ->
            case model.snakePosition of
                headPosition :: _ ->
                    if model.lastPositionWhenDirectionChanged == headPosition then
                        case model.direction of
                            Directions _ Nothing ->
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

        TogglePause visibility ->
            case visibility of
                Hidden ->
                    ( PlayingState { model | paused = True }, Cmd.none )

                Visible ->
                    ( PlayingState { model | paused = False }, Cmd.none )


getNewFruit : Cmd Msg
getNewFruit =
    Random.generate (PlayingType << NewFruit) (Random.pair (Random.int 1 (Tuple.first arenaDimension)) (Random.int 1 (Tuple.second arenaDimension)))


updateStartPage _ model =
    ( StartPageState model, Cmd.none )


updateGameOver : GameOverMsg -> GameOverModel -> ( Model, Cmd Msg )
updateGameOver msg _ =
    case msg of
        Restart ->
            ( initialPlayingState, Cmd.none )


outOfBound : ( Int, Int ) -> Bool
outOfBound ( x, y ) =
    x < 1 || x > Tuple.first arenaDimension || y < 1 || y > Tuple.second arenaDimension


ouroborosing : ( Int, Int ) -> List ( Int, Int ) -> Bool
ouroborosing ( x, y ) bodyParts =
    case bodyParts of
        [] ->
            False

        _ :: tail ->
            List.any (\( bodyPartX, bodyPartY ) -> x == bodyPartX && y == bodyPartY) tail



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
                        [ text "Game Over"
                        , button [ onClick (GameOverType Restart) ] [ text "restart" ]
                        , text ("Score: " ++ String.fromInt gameOverModel.score)
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

        _ =
            Debug.log "pos" snakePosition
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
                                                let
                                                    snakeHead =
                                                        if x == Tuple.first head && y == Tuple.second head then
                                                            [ style "background" "darkGreen" ]

                                                        else
                                                            []

                                                    snakeBody =
                                                        List.filterMap
                                                            (\( bodyPartX, bodyPartY ) ->
                                                                if x == bodyPartX && y == bodyPartY then
                                                                    Just (style "background" "green")

                                                                else
                                                                    Nothing
                                                            )
                                                            tail
                                                in
                                                snakeHead ++ snakeBody

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
                        [ pausedBoxView ]

                    else
                        []
                   )
            )
        , optionView difficulty
        ]


pausedBoxView : Html PlayingMsg
pausedBoxView =
    div
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


optionView : Int -> Html PlayingMsg
optionView difficulty =
    div []
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
                    , Browser.Events.onVisibilityChange (PlayingType << TogglePause)
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

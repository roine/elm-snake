module GameStates.Play exposing
    ( Model
    , Msg(..)
    , init
    , optionView
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (Visibility(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Leaderboard exposing (Leaderboard, UserScore, resetScore)
import Random
import Task
import Time exposing (Posix)


type alias Model =
    { snakePosition : List Position
    , lastTick : Posix
    , lastPositionWhenDirectionChanged : Position -- used to avoid more than one direction change per snake move
    , direction : Directions
    , difficulty : Int
    , fruitPosition : Maybe Position
    , paused : Bool
    , currentUserId : String
    , arenaDimension : ( Int, Int )
    , blockDimension : Int
    , leaderboard : Leaderboard
    }


init : String -> Leaderboard -> Model
init currentUserId leaderboard =
    { snakePosition = [ ( 10, 4 ), ( 9, 4 ) ]
    , lastTick = Time.millisToPosix 0
    , lastPositionWhenDirectionChanged = ( 10, 4 )
    , direction = Directions East Nothing
    , difficulty = 4
    , fruitPosition = Nothing
    , paused = False
    , currentUserId = currentUserId
    , arenaDimension = ( 30, 30 )
    , blockDimension = 15
    , leaderboard = resetScore currentUserId leaderboard
    }


type alias Position =
    ( Int, Int )


type Direction
    = West
    | North
    | East
    | South


oppositeDirection : Direction -> Direction -> Bool
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



-- Allow to register the next move


type Directions
    = Directions Direction (Maybe Direction)


getNextDirection (Directions direction _) =
    direction


getLastDirection (Directions _ direction) =
    direction


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
        , ( 4, ( 100, 300 ) )
        , ( 5, ( 50, 500 ) )
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
    = Tick Posix
    | ChangeDirection Direction
    | NewFruit Position
    | ChangeDifficulty String
    | StopPause
    | TogglePause Browser.Events.Visibility
    | ChangeState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            if Time.posixToMillis posix - Time.posixToMillis model.lastTick > getSpeedFromDifficulty model.difficulty then
                case model.snakePosition of
                    [] ->
                        ( model, Cmd.none )

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
                                        getNewFruit model

                                    Just fruitPosition ->
                                        if fruitPosition == head then
                                            getNewFruit model

                                        else
                                            Cmd.none
                        in
                        if outOfBound newPosition model || ouroborosing head model.snakePosition then
                            ( model, Task.perform (always ChangeState) (Task.succeed ()) )

                        else
                            ( { model
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
                                , leaderboard =
                                    Dict.update model.currentUserId
                                        (\maybeUser ->
                                            Maybe.map
                                                (\u ->
                                                    { u
                                                        | score =
                                                            case model.fruitPosition of
                                                                Just fruitPosition ->
                                                                    if fruitPosition == head then
                                                                        u.score + getScoreFromDifficulty model.difficulty + 1

                                                                    else
                                                                        u.score + 1

                                                                Nothing ->
                                                                    u.score + 1
                                                    }
                                                )
                                                maybeUser
                                        )
                                        model.leaderboard
                              }
                            , newFruit
                            )

            else
                ( model, Cmd.none )

        ChangeDirection newDirection ->
            case model.snakePosition of
                headPosition :: _ ->
                    if model.lastPositionWhenDirectionChanged == headPosition then
                        case model.direction of
                            Directions _ Nothing ->
                                ( { model
                                    | direction = Directions (getNextDirection model.direction) (Just newDirection)
                                  }
                                , Cmd.none
                                )

                            _ ->
                                ( model, Cmd.none )

                    else
                        ( { model
                            | direction = Directions newDirection Nothing
                            , lastPositionWhenDirectionChanged = headPosition
                          }
                        , Cmd.none
                        )

                [] ->
                    ( model, Cmd.none )

        NewFruit newPosition ->
            ( { model | fruitPosition = Just newPosition }, Cmd.none )

        ChangeDifficulty difficulty ->
            ( { model | difficulty = Maybe.withDefault model.difficulty (String.toInt difficulty), paused = True }, Cmd.none )

        StopPause ->
            ( { model | paused = False }, Cmd.none )

        TogglePause visibility ->
            case visibility of
                Hidden ->
                    ( { model | paused = True }, Cmd.none )

                Visible ->
                    ( { model | paused = False }, Cmd.none )

        ChangeState ->
            ( model, Cmd.none )


getNewFruit : Model -> Cmd Msg
getNewFruit { arenaDimension } =
    Random.generate NewFruit (Random.pair (Random.int 1 (Tuple.first arenaDimension)) (Random.int 1 (Tuple.second arenaDimension)))


outOfBound : ( Int, Int ) -> Model -> Bool
outOfBound ( x, y ) { arenaDimension } =
    x < 1 || x > Tuple.first arenaDimension || y < 1 || y > Tuple.second arenaDimension


ouroborosing : Position -> List Position -> Bool
ouroborosing ( x, y ) bodyParts =
    case bodyParts of
        [] ->
            False

        _ :: tail ->
            List.any (\( bodyPartX, bodyPartY ) -> x == bodyPartX && y == bodyPartY) tail



-- VIEW


view : Model -> Html Msg
view { snakePosition, fruitPosition, arenaDimension, paused, blockDimension, difficulty, leaderboard, currentUserId } =
    let
        columns =
            Tuple.first arenaDimension

        rows =
            Tuple.second arenaDimension

        width =
            (blockDimension * rows |> String.fromInt) ++ "px"

        height =
            (blockDimension * columns |> String.fromInt) ++ "px"

        user =
            Dict.get currentUserId leaderboard
    in
    div [ class "flex" ]
        [ div []
            [ div [ style "font-size" "40px", style "text-align" "center" ]
                [ case user of
                    Nothing ->
                        text ""

                    Just u ->
                        text (String.fromInt u.score)
                ]
            , div
                [ style "display" "flex" ]
                [ div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "height" height
                    , style "width" width
                    , style "border" "1px solid red"
                    , style "position" "relative"
                    , style "transition" "all 300ms ease"
                    ]
                    (List.map
                        (\y ->
                            div
                                [ style "display" "flex"
                                , style "width" width
                                , style "flex-grow" "1"
                                , style "transition" "all 300ms ease"
                                ]
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
                                                                    [ style "background" "darkGreen"
                                                                    , class "snake-head"
                                                                    , style "border-radius" " 0"
                                                                    ]

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
                                                            [ style "background" "linear-gradient(120deg, #f6d365 0%, #fda085 100%)"
                                                            , class "fruit"
                                                            , style "border-radius" " 100px"
                                                            ]

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
                ]
            ]
        , optionView difficulty
        ]


pausedBoxView : Html Msg
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


optionView : Int -> Html Msg
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


keyToDirections : String -> Maybe Direction
keyToDirections key =
    case Debug.log "dsf" key of
        "ArrowLeft" ->
            Just West

        "ArrowUp" ->
            Just North

        "ArrowRight" ->
            Just East

        "ArrowDown" ->
            Just South

        "a" ->
            Just West

        "w" ->
            Just North

        "d" ->
            Just East

        "s" ->
            Just South

        _ ->
            Nothing


keyDecoder : (Direction -> Msg) -> Model -> Json.Decode.Decoder Msg
keyDecoder tag model =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map keyToDirections
        |> Json.Decode.andThen
            (\newDirection ->
                case newDirection of
                    Nothing ->
                        Json.Decode.fail "Wrong key used"

                    Just direction ->
                        if direction == getNextDirection model.direction || oppositeDirection direction (getNextDirection model.direction) then
                            Json.Decode.fail "opposite direction or same direction"

                        else
                            Json.Decode.succeed direction
            )
        |> Json.Decode.map tag


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Sub.batch
            [ Browser.Events.onAnimationFrame Tick
            , Browser.Events.onKeyDown (keyDecoder ChangeDirection model)
            , Browser.Events.onVisibilityChange TogglePause
            ]

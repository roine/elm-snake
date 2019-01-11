module GameStates exposing (GameStatus(..), getUserScoreFromModel)

import GameStates.Over
import GameStates.Play
import GameStates.Start


type GameStatus
    = PlayingState GameStates.Play.Model
    | StartPageState GameStates.Start.Model
    | GameOverState GameStates.Over.Model


getUserScoreFromModel model =
    case model of
        StartPageState m ->
            m.leaderboard

        PlayingState m ->
            m.leaderboard

        GameOverState m ->
            m.leaderboard

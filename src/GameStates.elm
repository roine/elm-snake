module GameStates exposing (GameStatus(..), getLeaderboardFromModel, getUserScoreFromModel)

import GameStates.Over
import GameStates.Play
import GameStates.Start


type GameStatus
    = PlayingState GameStates.Play.Model
    | StartPageState GameStates.Start.Model
    | GameOverState GameStates.Over.Model


getLeaderboardFromModel model =
    case model of
        StartPageState m ->
            m.leaderboard

        PlayingState m ->
            m.leaderboard

        GameOverState m ->
            m.leaderboard


getUserScoreFromModel model =
    case model of
        StartPageState m ->
            { name = m.name, score = 0, uuid = m.uuid }

        PlayingState m ->
            { name = m.name, score = m.score, uuid = m.uuid }

        GameOverState m ->
            { name = m.name, score = m.score, uuid = m.uuid }

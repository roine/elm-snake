module GameStates exposing (GameStatus(..), getLeaderboardFromModel, getUserIdFromModel)

import GameStates.Over
import GameStates.Play
import GameStates.Start
import Leaderboard exposing (Leaderboard)


type GameStatus
    = PlayingState GameStates.Play.Model
    | StartPageState GameStates.Start.Model
    | GameOverState GameStates.Over.Model


getLeaderboardFromModel : GameStatus -> Leaderboard
getLeaderboardFromModel model =
    case model of
        StartPageState m ->
            m.leaderboard

        PlayingState m ->
            m.leaderboard

        GameOverState m ->
            m.leaderboard


getUserIdFromModel : GameStatus -> String
getUserIdFromModel model =
    case model of
        StartPageState m ->
            m.currentUserId

        PlayingState m ->
            m.currentUserId

        GameOverState m ->
            m.currentUserId

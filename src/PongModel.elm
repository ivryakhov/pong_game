module PongModel (..) where

import Time exposing (..)
import BallModel exposing (..)
import PlayerModel exposing (..)


type alias Input =
  { paddle1 : Int
  , paddle2 : Int
  , delta : Time
  }


type State
  = Play
  | Pause


type alias Game =
  { state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
  , board : Board
  }


type Action
  = ToggleState
  | Reset
  | MoveOn Input
  | NoOp


type alias Board =
  { halfWidth : Float
  , halfHeight : Float
  }


defaultBoard : Board
defaultBoard =
  { halfWidth = 400
  , halfHeight = 300
  }


defaultGame : Game
defaultGame =
  let
    board' =
      defaultBoard
  in
    { state = Pause
    , ball = defaultBall
    , player1 = player (20 - board'.halfWidth)
    , player2 = player (board'.halfWidth - 20)
    , board = board'
    }

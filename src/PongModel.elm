module PongModel (..) where

import Time exposing (..)
import BallModel exposing (..)
import PlayerModel exposing (..)


type alias Input =
  { paddle1 : Int
  , paddle2 : Int
  , delta : Time
  }

type State = Play | Pause

type alias Game =
  { state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
  }

type Action
  = ToggleState
  | Reset
  | MoveOn Input
  | NoOp

(gameWidth, gameHeight) = (600,400)
(halfWidth, halfHeight) = (300,200)

defaultGame : Game
defaultGame =
  { state   = Pause
  , ball    = defaultBall
  , player1 = player (20-halfWidth)
  , player2 = player (halfWidth-20)
  }


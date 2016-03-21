module PongModel (..) where

import Time exposing (..)


type alias Input =
  { space : Bool
  , paddle1 : Int
  , paddle2 : Int
  , delta : Time
  }

type alias Object a =
  { a |
        x : Float,
        y : Float,
        vx : Float,
        vy : Float
  }

type alias Ball =
  Object {}

type alias Player =
  Object { score : Int }

type State = Play | Pause


type alias Game =
  { state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
  }

(gameWidth, gameHeight) = (600,400)
(halfWidth, halfHeight) = (300,200)

player : Float -> Player
player x =
  { x=x, y=0, vx=0, vy=0, score=0 }

defaultGame : Game
defaultGame =
  { state   = Pause
  , ball    = { x=0, y=0, vx=200, vy=200 }
  , player1 = player (20-halfWidth)
  , player2 = player (halfWidth-20)
  }


module BallModel (..) where

import ObjectModel exposing (..)
import Graphics.Collage exposing (..)

type alias Ball =
  Object { baseWd : Float
         , baseHh : Float
         , isDirectionChanged : Bool
         , updateScore : (Int, Int)
         }

defaultBall : Ball
defaultBall = { x = 0
              , y = 0
              , vx = 100
              , vy = 100
              , width = 15
              , height = 15
              , baseWd = 15
              , baseHh = 15
              , isDirectionChanged = False
              , updateScore = (0, 0)
              }

ballShape : Ball -> Shape
ballShape ball =
  oval ball.width ball.height

module PlayerModel (..) where

import ObjectModel exposing (..)
import Graphics.Collage exposing (..)


type alias Player =
  Object { score : Int }


player : Float -> Player
player x =
  { x=x, y=0, vx=0, vy=0, width = 20, height = 60, score = 0 }

playerShape : Player -> Shape
playerShape player = 
  rect player.width player.height

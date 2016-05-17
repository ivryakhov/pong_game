module PlayerModel (..) where

import ObjectModel exposing (..)
import Graphics.Collage exposing (..)


type alias Player =
  Object { score : Int, tanA : Float }


player : Float -> Player
player x =
  let
    hh = 60
    wd = 20
    tanA = hh/wd
  in
    { x=x, y=0, vx=0, vy=0, width = wd, height = hh, score = 0, tanA = tanA }

playerShape : Player -> Shape
playerShape player = 
  rect player.width player.height

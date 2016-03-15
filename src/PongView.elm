module PongView where

import Color exposing (..)
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import PongModel exposing (..)

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = leftAligned << f << Text.monospace << Text.color textGreen << Text.fromString
msg = "SPACE to start, WS and &uarr;&darr; to move"


displayObj : Object a -> Shape -> Form
displayObj obj shape =
  move (obj.x, obj.y) (filled white shape)


display : (Int, Int) -> Game -> Element
display (w,h) {state,ball,player1,player2} =
  container w h middle <|
      collage gameWidth gameHeight
          [ filled pongGreen (rect gameWidth gameHeight)
          , displayObj ball  (oval 15 15)
          , displayObj player1 (rect 10 40)
          , displayObj player2 (rect 10 40)
          , toForm (scores player1 player2)
            |> move (0, gameHeight/2 - 40)
          , toForm (if state == Play then
                      spacer 1 1
                    else
                      txt identity msg)
            |> move (0, 40 - gameHeight/2)
          ]

scores : Player -> Player -> Element
scores player1 player2 = 
  toString player1.score ++ "  " ++ toString player2.score
    |> txt (Text.height 50)

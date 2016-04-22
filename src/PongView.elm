module PongView (..) where

import Color exposing (..)
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import ObjectModel exposing (..)
import PongModel exposing (..)
import BallModel exposing (..)
import PlayerModel exposing (..)


pongGreen =
  rgb 125 221 79


pongPurple =
  rgb 186 67 160


pongOrange =
  rgb 255 150 91


pongRed =
  rgb 238 85 115


pongBoard =
  { color = pongGreen
  , width = 5
  , cap = Flat
  , join = Smooth
  , dashing = []
  , dashOffset = 0
  }


dottedLine =
  { color = pongGreen
  , width = 2
  , cap = Flat
  , join = Smooth
  , dashing = [ 4 ]
  , dashOffset = 0
  }


txt f =
  leftAligned << f << Text.monospace << Text.color pongPurple << Text.fromString


msg1 =
  "SPACE to start, WS and &uarr;&darr; to move"


msg2 =
  "SPACE to pause the game, Ctrl for game reset"


displayObj : Object a -> Shape -> Color -> Form
displayObj obj shape color =
  move ( obj.x, obj.y ) (filled color shape)


display : ( Int, Int ) -> Game -> Element
display ( w, h ) { state, ball, player1, player2, board } =
  let
    gameWidth =
      board.halfWidth * 2

    gameHeight =
      board.halfHeight * 2
  in
    container w h middle
      <| collage
          gameWidth
          gameHeight
          [ outlined pongBoard (rect gameWidth gameHeight)
          , traced dottedLine (segment ( -gameWidth, 0 ) ( gameWidth, 0 ))
            -- TODO: generalize shapes building
          , displayObj ball (ballShape ball) pongRed
          , displayObj player1 (playerShape player1) pongPurple
          , displayObj player2 (playerShape player2) pongPurple
          , toForm (scores player1 player2)
              |> move ( 0, gameHeight / 2 - 40 )
          , toForm
              (if state == Play then
                txt identity msg2
               else
                txt identity msg1
              )
              |> move ( 0, 40 - gameHeight / 2 )
          ]


scores : Player -> Player -> Element
scores player1 player2 =
  toString player1.score
    ++ "  "
    ++ toString player2.score
    |> txt (Text.height 50)

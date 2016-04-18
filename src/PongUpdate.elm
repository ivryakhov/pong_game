module PongUpdate where

import PongModel exposing (..)
import BallModel exposing (..)
import PlayerModel exposing (..)
import ObjectModel exposing (..)
import Time exposing (..)
import Keyboard
import AnimationFrame exposing (frame)
import Debug exposing (..)

gameState : Signal Game
gameState =
  Signal.foldp stepGame defaultGame input


stepGame : Action -> Game -> Game
stepGame action game =
  case action of
    ToggleState ->
      toggleGameState game

    Reset ->
      defaultGame

    MoveOn input ->
      case game.state of
        Pause -> game
        Play  -> moveOn input game

    NoOp ->
      game

moveOn : Input -> Game -> Game
moveOn input game =
  let 
    { paddle1, paddle2, delta } = input
    { state, ball, player1, player2 } = game

    score1 = 
      if ball.x > halfWidth then 1 else 0

    score2 = 
      if ball.x < -halfWidth then 1 else 0

    ball' =
      stepBall delta ball player1 player2
                 
    player1' =
      stepPlayer delta paddle1 score1 player1

    player2' =
      stepPlayer delta paddle2 score2 player2
  in
    { game |
        ball = ball',
        player1 = player1',
        player2 = player2'
    }
      
stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({x,y,vx,vy, width, height, baseWd,  baseHh, prevVx, prevVy} as ball) player1 player2 =
  let
    xt = Debug.watch "xt" <| x + vx * t
    yt = Debug.watch "yt" <| y + vy * t

    (x',y') = Debug.watch "(x', y')" <|
      case abs(xt) >= halfWidth of
        True -> (defaultBall.x, defaultBall.y)
        False -> (xt, yt)

    delta' = Debug.watch "delta" <| t
    targetPlayer =
      case x' <= 0 of
        True -> player1
        False -> player2

    xColl = Debug.watch "xColl" <| 
            calcXColl targetPlayer x' y' baseWd baseHh
    yColl = Debug.watch "yColl" <|
            halfHeight - 5/2 - (abs(y') + baseHh/2) 

    (vx', xWdt, xHdt) = Debug.watch "vx'" <|
      case xColl <= 0 && prevVx == vx of
        True -> (-vx, xColl, -xColl)
        False -> (vx, 0, 0)
    
    prevVx' = vx

    (vy', yWdt, yHdt) = Debug.watch "vy" <|
      case yColl <= 0 && prevVy == vy of
        True -> (-vy, -yColl, yColl)
        False -> (vy, 0, 0)

    prevVy' = vy

  in
    { ball 
      | x = x'
      , y = y'
      , vx = vx'
      , vy = vy'
      , prevVx = prevVx'
      , prevVy = prevVy'
      , width = baseWd + xWdt + yWdt
      , height = baseHh + xHdt + yHdt
    }

    

calcXColl : Player -> Float -> Float -> Float -> Float -> Float
calcXColl player x y ballWd ballHh =
  case y < player.y + player.height/2 && y > player.y - player.height/2 of
    True ->  (abs(player.x) - player.width / 2) - (abs(x) + ballWd/2)
    False -> 10




{-    ball' = 
      { ball |
               vx = stepV vx (ball `within` player1)
                             (ball `within` player2),
               vy = stepV vy (y < 10 - halfHeight)
                             (y > halfHeight - 10)
      }
  in
    if not (ball.x |> near 0 halfWidth) then
      { ball |
               x=0,
               y=0
      }
    else stepObj t ball' -}

stepPlayer : Time -> Int -> Int -> Player -> Player
stepPlayer t dir points player =
  let player' = stepObj t { player | vy = toFloat dir * 200 }
      y' = clamp (22-halfHeight) (halfHeight-22) player'.y
      score' = player.score + points
  in
    { player' | y = y', score = score' }


stepObj : Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
  { obj |
          x = x + vx * t,
          y = y + vy * t
  }

stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if lowerCollision then
    abs v
  else if upperCollision then
         -(abs v)
       else v

delta : Signal Time
delta =
  Signal.map inSeconds frame


getSpace : Signal Action
getSpace =
  let
    ifToggleState b =
      case b of
        True -> ToggleState
        False -> NoOp
  in
    Signal.map ifToggleState  Keyboard.space

getCtrl : Signal Action
getCtrl =
  Signal.map (always Reset) Keyboard.ctrl


getInput : Signal Action
getInput =
  Signal.map3 inputToAction
          (Signal.map .y Keyboard.wasd)
          (Signal.map .y Keyboard.arrows)
          delta

inputToAction : Int -> Int -> Time -> Action
inputToAction a b c =
  MoveOn (Input a b c)

input : Signal Action
input =
  Signal.mergeMany [ getSpace, getInput, getCtrl ]


-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m =
  m >= n-c && m <= n+c


-- is the ball within a paddle?
within : Ball -> Player -> Bool
within ball player =
  near player.x 8 ball.x 
  && near player.y 20 ball.y

toggleGameState : Game -> Game
toggleGameState game = 
  let
    state' =
      case game.state of
        Play  -> Pause
        Pause -> Play
    
  in
    { game | state = state' }


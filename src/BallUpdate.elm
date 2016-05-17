module BallUpdate (..) where

import PongModel exposing (..)
import BallModel exposing (..)
import PlayerModel exposing (..)
import Time exposing (..)
import Debug exposing (..)

stepBall : Time -> Ball -> Ball -> Player -> Player -> Board -> Ball
stepBall t ball defaultBall player1 player2 board =
  let
    { x, y, vx, vy, width, height, baseWd, baseHh, isDirectionChanged, updateScore } = ball
      
    xt = Debug.watch "xt" <| x + vx * t
    yt = Debug.watch "yt" <| y + vy * t

    targetPlayer =
      case xt  <= 0 of
        True ->
          player1
        False ->
          player2

    xCorrection = Debug.watch "xCorrection" <|
                  targetPlayer.width/2 + baseWd/2
    xBallCollidePoint = Debug.watch "xBallCollidePoint" <|
                        case targetPlayer.x >= 0 of
                          True -> targetPlayer.x - xCorrection
                          False -> targetPlayer.x + xCorrection

    scores =
      case xBallCollidePoint >= 0 of
        True -> (1, 0)
        False -> (0, 1)

    xPadCollide = abs(xt) - abs(xBallCollidePoint)
    yHorisontCollide =  abs(yt) -  (board.halfHeight - baseHh/2 - board.halfBorderWidth)
                        
    isBallOverVerticalBorder = Debug.watch "isBallOverVerticalBorder" <|
                               abs(xt) >= board.halfWidth - baseHh/2 - board.halfBorderWidth
    isBallCollideWithPadOnX = Debug.watch "isBallCollideWithPadOnX" <|
                              xPadCollide >= 0 
    isBallCollideWithPadOnY = Debug.watch "isBallCollideWithPadOnY" <|
                              ( yt <= targetPlayer.y + targetPlayer.height/2 + baseHh/2 ) &&
                              ( yt >= targetPlayer.y - targetPlayer.height/2 - baseHh/2 )
    isBallCollideWithHorisontalBorders = Debug.watch "isBallCollideWithHorisontalBorders" <|
                                         yHorisontCollide >= 0
                                                                             
 
    ( x', y', vx', vy', width', height', isDirectionChanged', updateScore' ) =
      case ( isBallOverVerticalBorder
           , isBallCollideWithPadOnX
           , isBallCollideWithPadOnY
           , isBallCollideWithHorisontalBorders
           , isDirectionChanged
           ) of
        ( True, _, _, _, _ ) ->
          ( defaultBall.x, defaultBall.y, defaultBall.vx, defaultBall.vy, baseWd, baseHh, False, scores )
        ( False, True, True, False, False ) -> ( xt, yt, -vx, vy, baseWd - xPadCollide, baseHh + xPadCollide, True, defaultBall.updateScore )
        ( _, _, _, True, False) -> ( xt, yt, vx, -vy, baseWd + yHorisontCollide, baseHh - yHorisontCollide, True, defaultBall.updateScore )
        ( _, _, _, _, _) -> (xt, yt, vx, vy, baseWd, baseHh, False, defaultBall.updateScore )

  in
    { ball
      | x = x'
      , y = y'
      , vx = vx'
      , vy = vy'
      , width = width'
      , height = height'
      , updateScore = updateScore'
    }

     




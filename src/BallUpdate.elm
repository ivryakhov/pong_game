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
    
    -- ball coordinates at the end of the step
    xt = Debug.watch "xt" <| x + vx * t
    yt = Debug.watch "yt" <| y + vy * t

    -- taking in mind only one player instead of two depend of board's side
    targetPlayer =
      case xt  <= 0 of
        True ->
          player1
        False ->
          player2

    -- taking in mind ball's radius and pad's size
    xCorrection = targetPlayer.width/2 + baseWd/2
    yCorrection = targetPlayer.height/2 + baseHh/2

    -- actual point of intersection of ball with pad
    xBallCollideWithPad = Debug.watch "xBallCollideWithPad" <|
                        case targetPlayer.x >= 0 of
                          True  -> targetPlayer.x - xCorrection
                          False -> targetPlayer.x + xCorrection
    yBallCollideWithPad = Debug.watch "yBallCollideWithPad" <|
                       case ball.y > targetPlayer.y of
                         True  -> targetPlayer.y + yCorrection
                         False -> targetPlayer.y - yCorrection 
                                  
    -- distance between step and collide points
    xStepCollide = abs(xt) - abs(xBallCollideWithPad)
    yStepCollide = abs(yBallCollideWithPad) - abs(yt)

    scores =
      case xBallCollideWithPad >= 0 of
        True -> (1, 0)
        False -> (0, 1)


    yHorisontCollide =  abs(yt) -  (board.halfHeight - baseHh/2 - board.halfBorderWidth)
                        
    isBallOverVerticalBorder = Debug.watch "isBallOverVerticalBorder" <|
                               abs(xt) >= board.halfWidth - baseHh/2 - board.halfBorderWidth
    isBallCollideWithPadOnX = Debug.watch "isBallCollideWithPadOnX" <|
                              xStepCollide >= 0 
    isBallCollideWithPadOnY = Debug.watch "isBallCollideWithPadOnY" <|
                              yStepCollide >= 0
    isBallCollideWithHorisontalBorders = Debug.watch "isBallCollideWithHorisontalBorders" <|
                                         yHorisontCollide >= 0
    
    -- tan between centers of ball and target pad
--    tanBP = 
                                                                             
 
    ( x', y', vx', vy', width', height', isDirectionChanged', updateScore' ) =
      case ( isBallOverVerticalBorder
           , isBallCollideWithPadOnX
           , isBallCollideWithPadOnY
           , isBallCollideWithHorisontalBorders
           , isDirectionChanged
           ) of
        ( True, _, _, _, _ ) ->
          ( defaultBall.x, defaultBall.y, defaultBall.vx, defaultBall.vy, baseWd, baseHh, False, scores )
        ( False, True, True, False, False ) -> ( xt, yt, -vx, vy, baseWd - xStepCollide, baseHh + xStepCollide, True, defaultBall.updateScore )
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

     




module Pong where

import Window
import PongModel exposing (..)
import PongUpdate exposing (..)
import PongView exposing (..)

main =
    Signal.map2 display Window.dimensions gameState

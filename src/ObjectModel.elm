module ObjectModel (..) where

type alias Object a =
  { a | x : Float
      , y : Float
      , vx : Float
      , vy : Float
      , width : Float
      , height : Float
  }


module Tc170_Aux where

class ReadMode mode

data Attr m w a = Attr (w -> IO a) (w -> a -> IO ())  

mapAttr :: ReadMode m => (a -> b) -> (a -> b -> a) -> Attr m w a -> Attr m w b
mapAttr get set (Attr getter setter)
    = Attr (\w   -> do a <- getter w; return (get a))
           (\w b -> do a <- getter w; setter w (set a b))


data Rect  = Rect
data Point = Point
topLeft    = undefined
rectMoveTo = undefined

class Dimensions w where
  frame    :: ReadMode m => Attr m w Rect

  position :: ReadMode m => Attr m w Point
  position  = mapAttr (\f -> topLeft f) (\f p -> rectMoveTo p f) frame


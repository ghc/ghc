module Simpl020_A where

class GUIValue a

class GUIObject w where
  toGUIObject     :: w -> ()
  cset            :: GUIValue a => a -> w

instance GUIValue Int

class GUIObject w => HasSize w where
  width :: Int -> w

class HasSize w => HasGeometry w where
  geometry :: Int -> w

class GUIObject w => Window w where

instance Window w => HasSize w where
  width w = geometry w

instance Window w => HasGeometry w where
  geometry g = cset g

instance GUIObject ()

instance Window ()

{-# LANGUAGE LinearTypes #-}
module GuardTup where

data Q = Q ()

mkQ :: () -> Q
mkQ = Q

foo smart
  | smart = mkQ
  | otherwise = Q

fooIf smart = if smart then mkQ else Q

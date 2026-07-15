module T27360 where

import GHC.Exts

f :: ()
f | False, considerAccessible = ()
  | otherwise = ()

g :: ()
g | False, True <- considerAccessible = ()
  | otherwise = ()

module KeepCafs1 where

import KeepCafsBase

foreign export ccall "getX"
  getX :: IO Int

getX :: IO Int
getX = return x

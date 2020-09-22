{-# LANGUAGE DataKinds #-}
module T18723b where

import Data.Proxy

data T2 = MkT2 (Proxy
 '( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int, Int, Int, Int, Int, Int, Int, Int
  , Int, Int, Int
  ))

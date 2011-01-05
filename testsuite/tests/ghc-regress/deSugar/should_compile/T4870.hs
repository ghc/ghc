module T4870 where

import T4870a

data D = D

instance C D where
  c x = x

{-# SPECIALIZE f :: D #-}

{-# LANGUAGE TypeFamilies #-}

module T27828a where

import Data.Proxy

type family F a where
  F Int = Int   -- B1
  F Char = Bool -- B2
  F a   = a     -- B3

h :: (F a ~ Int) => Proxy a -> ()
h = error "urk"

k :: ()
k = h Proxy      -- [W] F alpha ~ Int

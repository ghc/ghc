{-# LANGUAGE TypeInType #-}
module Bug where

import T13915a_Foo

data Proxy (a :: k)
data S = MkS (Proxy 'MkT)

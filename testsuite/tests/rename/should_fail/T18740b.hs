{-# LANGUAGE ScopedTypeVariables #-}
module T18740b where

import Data.Proxy

f (Proxy :: Proxy a) = a

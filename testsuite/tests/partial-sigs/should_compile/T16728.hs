{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bug where

import Data.Proxy

f :: forall k (x :: k). Proxy (x :: _)
f = Proxy

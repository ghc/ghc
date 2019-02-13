{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Bug where

import Data.Proxy

f2 :: forall a (x :: a). Proxy (x :: _)
f2 = Proxy

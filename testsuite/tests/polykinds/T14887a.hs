{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-implicit-kind-vars #-}
module Bug where

import Data.Proxy

f1 :: forall (x :: a). Proxy (x :: _)
-- This one has an implicitly-quantified kind var 'a', which
-- we will stop accepting in the future, under the forall-or-nothing
-- rule.  Hence -Wno-implicit-kind-vars
f1 = Proxy

f2 :: forall a (x :: a). Proxy (x :: _)
f2 = Proxy

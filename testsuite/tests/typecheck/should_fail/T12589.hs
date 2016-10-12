{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
module T12589 where

import Data.Proxy

hcpure :: proxy c -> (forall a. c a => f a) -> h f xs
hcpure _ _ = undefined

a = minBound
  & hcpure (Proxy @Bounded)

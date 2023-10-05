{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
module T17360 where

import GHC.Exts

newtype Id (a :: TYPE r) = Id a

foo :: forall r (a :: TYPE r). Id a -> Id a
foo x = x

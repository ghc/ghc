{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}

module GHC.IO where

import GHC.Prim
import GHC.Types
import {-# SOURCE #-} GHC.Exception.Type (SomeException)

mplusIO :: IO a -> IO a -> IO a
mkUserError :: [Char] -> SomeException
raiseIO# :: forall (l :: Levity) (r :: RuntimeRep) (a :: TYPE ('BoxedRep l)) (b :: TYPE r). a -> State# RealWorld -> (# State# RealWorld, b #)

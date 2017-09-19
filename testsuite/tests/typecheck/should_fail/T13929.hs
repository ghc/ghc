{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import GHC.Exts
import Data.Kind
import GHC.Generics

class GUnbox (f :: Type -> Type) (r :: RuntimeRep) where
    type GUnboxed f r :: TYPE r
    gunbox :: f p -> GUnboxed f r

instance (GUnbox f rf, GUnbox g rg) => GUnbox (f :*: g) ('TupleRep '[rf, rg]) where
    type GUnboxed (f :*: g) ('TupleRep '[rf, rg]) = (# GUnboxed f rf, GUnboxed g rg #)
    -- if I remove implementation of `gunbox` it compiles successfully
    gunbox (x :*: y) = (# gunbox x, gunbox y #)

instance (GUnbox f rf, GUnbox g rg) => GUnbox (f :+: g) ('SumRep '[rf, rg]) where
    type GUnboxed (f :+: g) ('SumRep '[rf, rg]) = (# GUnboxed f rf | GUnboxed g rg #)
    gunbox (L1 l) = (# gunbox l | #)
    gunbox (R1 r) = (# | gunbox r #)

main :: IO ()
main = pure ()

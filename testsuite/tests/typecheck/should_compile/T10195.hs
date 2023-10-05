{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GADTs,
             ConstraintKinds, DataKinds, KindSignatures,
             FlexibleInstances #-}

module T10195 where

import Data.Kind

data Foo m zp r'q = Foo zp
data Dict :: Constraint -> Type where
  Dict :: a => Dict a

type family BarFamily a b :: Bool
class Bar m m'
instance (BarFamily m m' ~ 'True) => Bar m m'

magic :: (Bar m m') => c m zp -> Foo m zp (c m' zq)
-- Weird test case: (Bar m m') is simplifiable
magic = undefined

getDict :: a -> Dict (Num a)
getDict _ = undefined
fromScalar :: r -> c m r
fromScalar = undefined

foo :: (Bar m m')
  => c m zp -> Foo m zp (c m' zq) -> Foo m zp (c m' zq)
-- Weird test case: (Bar m m') is simplifiable
foo b (Foo sc) =
  let scinv = fromScalar sc
  in case getDict scinv of
    Dict -> magic $ scinv * b

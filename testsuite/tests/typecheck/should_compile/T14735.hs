{-# Language QuantifiedConstraints #-}
{-# Language StandaloneDeriving #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language GADTs #-}
{-# Language KindSignatures #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds #-}

module T14735 where

import Data.Kind

data D c where
  D :: c => D c

newtype a :- b = S (a => D b)

class C1 a b
class C2 a b
instance C1 a b => C2 a b

class    (forall xx. f xx) => Limit f
instance (forall xx. f xx) => Limit f

impl :: Limit (C1 a) :- Limit (C2 a)
impl = S D

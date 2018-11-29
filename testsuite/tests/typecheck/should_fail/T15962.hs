{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module T15962 where

import Data.Kind (Type)

type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

data OpKind = Conjunction

data Dual (k :: OpKind) :: Exp OpKind

data Map :: (a -> Exp b) -> [ a ] -> Exp [ b ]

type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

data Big :: [ OpKind ] -> Type where
  Big  :: [ Big ks ] -> Big ('Conjunction ': ks)

dualBig :: Big ks -> Big (Eval (Map Dual ks))
dualBig = _

instance Semigroup (Big a) where
  Big xs <> Big ys = Big (xs <> ys)

instance Monoid (Big ('Conjunction ': ks)) where
  mempty = iDontExist

flatten :: Monoid (Big ks) => Big (k ': k ': ks) -> Big ks
flatten = undefined

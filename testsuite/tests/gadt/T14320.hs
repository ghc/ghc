{-# LANGUAGE RankNTypes, GADTs, KindSignatures #-}
module T14320
where

import Data.Kind (Type)

data Exp :: Type where
  Lit  :: (Int -> Exp)

newtype TypedExp :: Type -> Type where
  TEGood ::  forall a . (Exp -> (TypedExp a))

-- The presence of outer parentheses makes the `forall` nested, and
-- GADTs do not permit nested `forall`s.
--
newtype TypedExpToo :: Type -> Type where
  TEBad  :: (forall a . (Exp -> (TypedExpToo a)))

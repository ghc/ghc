{-# LANGUAGE RankNTypes, GADTs, KindSignatures #-}
module T14320
where

import Data.Kind (Type)

data Exp :: Type where
  Lit  :: (Int -> Exp)

newtype TypedExp :: Type -> Type where
  TEGood ::  forall a . (Exp -> (TypedExp a))

-- The only difference here is that the type is wrapped in parentheses,
-- but GHC 8.0.1 rejects this program
--
newtype TypedExpToo :: Type -> Type where
  TEBad  :: (forall a . (Exp -> (TypedExpToo a)))

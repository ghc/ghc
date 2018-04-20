{-# LANGUAGE RankNTypes, GADTs, KindSignatures #-}
module T14320
where

data Exp :: * where
  Lit  :: (Int -> Exp)

newtype TypedExp :: * -> * where
  TEGood ::  forall a . (Exp -> (TypedExp a))

-- The only difference here is that the type is wrapped in parentheses,
-- but GHC 8.0.1 rejects this program
--
newtype TypedExpToo :: * -> * where
  TEBad  :: (forall a . (Exp -> (TypedExpToo a)))

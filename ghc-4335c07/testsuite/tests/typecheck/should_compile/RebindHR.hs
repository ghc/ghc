{-# LANGUAGE RebindableSyntax, GADTs, RankNTypes, TypeOperators, ScopedTypeVariables #-}

module RebindHR where

import Prelude hiding ( (>>=) )
import Data.Typeable

data Exp = Int Int | Plus Exp Exp | Bool Bool
data TExp a where
  TInt :: Int -> TExp Int
  TPlus :: TExp Int -> TExp Int -> TExp Int
  TBool :: Bool -> TExp Bool

(>>=) :: ((forall t. Typeable t => TExp t -> Maybe r) -> Maybe r)
      -> (forall t. Typeable t => TExp t -> Maybe r)
      -> Maybe r
x >>= y = x y

check :: Exp -> (forall t. Typeable t => TExp t -> Maybe r) -> Maybe r
check (Int n) k = k (TInt n)
check (Bool b) k = k (TBool b)
check (Plus e1 e2) k = do te1 :: TExp ty1 <- check e1
                          te2 :: TExp ty2 <- check e2
                          case (eqT :: Maybe (ty1 :~: Int), eqT :: Maybe (ty2 :~: Int)) of
                            (Just Refl, Just Refl) -> k (TPlus te1 te2)
                            _ -> Nothing

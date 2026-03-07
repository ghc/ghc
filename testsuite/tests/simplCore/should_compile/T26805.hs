{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeData           #-}
module T26805( interpret )  where

import Data.Kind (Type)

data Phantom (sh :: Type) = Phantom  -- newtype fails to specialise as well

instance Show (Phantom sh) where
  show Phantom = "show"

type Foo r = (forall sh. Show (Phantom sh), Num r)
-- this specialises fine:
-- type Foo r = (Num r)

type data TK = TKScalar Type

data AstTensor :: TK -> Type where
  AstInt :: Int -> AstTensor (TKScalar Int)
  AstPlus :: Foo r => AstTensor (TKScalar r) -> AstTensor (TKScalar r)

plusConcrete :: Foo r => r -> r
plusConcrete = (+ 1)

interpret :: AstTensor (TKScalar Int) -> Int
interpret v0 = case v0 of
  AstInt n  -> n
  AstPlus u -> plusConcrete (interpret u)

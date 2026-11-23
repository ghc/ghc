{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module TypeLambdaArgument (messageCells, rowFor) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Matchability (U)
import TypeLambda (Fields(..))

-- | Build a two-column row at the selected column types.
rowFor :: forall (f :: Type %U -> Type). Proxy f -> f Int -> f Bool -> Fields '[f Int, f Bool]
rowFor _ count flag = Field count (Field flag End)

-- | Decode a count and report the missing flag with the caller's error type.
messageCells :: forall (e :: Type). e -> Fields '[Either e Int, Either e Bool]
messageCells msg = rowFor @(\cell -> Either e cell) Proxy (Right 42) (Left msg)

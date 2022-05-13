{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import GHC.Exts (withDict)
import GHC.TypeLits (Symbol)
import GHC.Exts

newtype SSymbol (s :: Symbol) = SSymbol String

class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n

symbolVal :: forall n proxy . KnownSymbol n => proxy n -> String
symbolVal _ = case symbolSing :: SSymbol n of SSymbol x -> x

-- See Note [NOINLINE someNatVal] in GHC.TypeNats
{-# NOINLINE reifySymbol #-}
reifySymbol :: forall r. String -> (forall (n :: Symbol). KnownSymbol n => Proxy n -> r) -> r
reifySymbol n k = withDict @(KnownSymbol Any) @(SSymbol Any) (SSymbol n) (k @Any) (Proxy @(Any @Symbol))

main :: IO ()
main = print $ reifySymbol "Hello World" symbolVal

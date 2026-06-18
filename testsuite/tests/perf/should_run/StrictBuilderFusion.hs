{-# LANGUAGE BangPatterns #-}

-- A strict field of function type (here a ByteString 'Builder', a newtype over a
-- CPS function) must stay fused: the worker for 'build' returns a bare 'Builder'
-- and the wrapper reboxes once, rather than the worker returning a 'Strict
-- Builder' that is reboxed on every iteration.
-- See Note [Unboxing Strict boxes around functions] in GHC.Core.Opt.CprAnal.

module Main where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L

data PairS = PairS () !Builder

newtype P = P PairS

tell :: Builder -> P
tell b = P (PairS () b)
{-# INLINE tell #-}

andThen :: P -> P -> P
andThen (P (PairS _ b1)) (P (PairS _ b2)) = P (PairS () (b1 <> b2))
{-# INLINE andThen #-}

run :: P -> Builder
run (P (PairS _ b)) = b

build :: Int -> P
build 0 = tell mempty
build k = tell (word8 5) `andThen` build (k - 1)

main :: IO ()
main = print (L.length (toLazyByteString (run (build 100000))))

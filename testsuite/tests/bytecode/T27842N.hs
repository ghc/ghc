{-# OPTIONS_GHC -fno-byte-code-and-object-code #-}
-- We compile this module as native code, depending on A's interface file.
-- A itself is loaded into GHCi, despite it's interface file being generated based
-- on native code generation.
module T27842N (S(..), use_S, M(..), use_M, N(..), use_N) where

data S = S ![Int]

-- We take apart the strict constructor value and branch on the value in its field.
-- If the field content is not EPT this causes undefined behaviour.

use_S :: S -> Int
use_S (S xs) = case xs of { (y:_) -> y; [] -> -1 }
{-# NOINLINE use_S #-}

data M = M !(Maybe Int)

use_M :: M -> Int
use_M (M m) = case m of { Just x -> x; Nothing -> -1 }
{-# NOINLINE use_M #-}

data N = N !Ordering

use_N :: N -> Int
use_N (N o) = case o of { LT -> 1; EQ -> 2; GT -> 3 }
{-# NOINLINE use_N #-}

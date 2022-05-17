{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.ControlFlow
  ( WasmControl(..)
  , BrTableInterval(..), inclusiveInterval
  , brTableLimit
  )
where

import GHC.Prelude

import Data.Semigroup

import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic

{-|
Module      : GHC.Wasm.ControlFlow
Description : Representation of control-flow portion of the WebAssembly instruction set
-}

-- [Note block types]
--
-- WebAssembly blocks are normally labeled with a function type,
-- which specifies what values the block expects to find on the
-- WebAssembly evaluation stack and what values it promises to
-- leave there.  Those types do not appear in this representation.
-- The representation assumes that either the stack is left
-- unchanged by every block (Wasm type `[] -> []`) or that if
-- other types are needed, they will be computed by running
-- an inference algorithm over the code.


-- | Representation of WebAssembly control flow.
-- Type parameter `s` is the type of (unspecified) statements.
-- It might be instantiated with an open Cmm block or with a sequence
-- of Wasm instructions.
-- Parameter `e` is the type of expressions.

data WasmControl s e where

  WasmBlock :: WasmControl s e -> WasmControl s e
  WasmLoop  :: WasmControl s e -> WasmControl s e
  WasmIf    :: e -> WasmControl s e -> WasmControl s e -> WasmControl s e

  WasmBr   :: Int -> WasmControl s e
  WasmBrTable :: e
              -> BrTableInterval -- for testing
              -> [Int]           -- targets
              -> Int             -- default target
              -> WasmControl s e
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturn :: WasmControl s e

  WasmAction :: s -> WasmControl s e   -- basic block: one entry, one exit
  WasmSeq :: WasmControl s e -> WasmControl s e -> WasmControl s e

  WasmUnreachable :: WasmControl s e
  WasmNop :: WasmControl s e

data BrTableInterval
    = BrTableInterval { bti_lo :: Integer, bti_count :: Integer }
  deriving (Show)

instance Outputable BrTableInterval where
  ppr range = brackets $ hcat[integer (bti_lo range), text "..", integer hi]
      where hi = bti_lo range + bti_count range - 1

inclusiveInterval :: Integer -> Integer -> BrTableInterval
inclusiveInterval lo hi
    | lo <= hi = BrTableInterval lo (hi - lo + 1)
    | otherwise = panic "GHC.Wasm.ControlFlow: empty interval"

instance Semigroup (WasmControl s e) where
  WasmNop <> a = a
  a <> WasmNop = a
  a <> b = WasmSeq a b

instance Monoid (WasmControl s e) where
  mempty = WasmNop


brTableLimit :: Int
  -- ^ Size of the largest table that is deemed acceptable in a `br_table` instruction.
  --
  -- Source: https://chromium.googlesource.com/v8/v8/+/master/src/wasm/wasm-limits.h#51
  -- See also discussion at https://github.com/WebAssembly/spec/issues/607, which shows
  -- that major browsers agree.
brTableLimit = 65520

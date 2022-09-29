{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Wasm.IR
  ( WasmIR(..), (<>), pattern WasmIf, wasmReturn
  , BrTableInterval(..), inclusiveInterval
  , brTableLimit

  , WasmType(..), WasmTypeTag(..)
  , TypeList(..)
  , WasmFunctionType(..)

  , SymName(..)
  )
where

import GHC.Prelude

import Data.Kind

import GHC.Data.FastString
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic

{-|
Module      : GHC.Wasm.ControlFlow
Description : Representation of control-flow portion of the WebAssembly instruction set
-}

-- | WebAssembly type of a WebAssembly value that WebAssembly code
-- could either expect on the evaluation stack or leave on the evaluation
-- stack.

data WasmType = I32 | F32 | I64 | F64
  deriving (Eq, Show)


-- | Singleton type useful for programming with `WasmType` at the type level.

data WasmTypeTag :: WasmType -> Type where
  TagI32 :: WasmTypeTag 'I32
  TagF32 :: WasmTypeTag 'F32
  TagI64 :: WasmTypeTag 'I64
  TagF64 :: WasmTypeTag 'F64

-- | List of WebAssembly types used to describe the sequence of WebAssembly
-- values that a block of code may expect on the stack or leave on the stack.

data TypeList :: [WasmType] -> Type where
  TypeListNil :: TypeList '[]
  TypeListCons :: WasmTypeTag t -> TypeList ts -> TypeList (t : ts)

-- | The type of a WebAssembly function, loop, block, or conditional.
-- This type says what values the code expects to pop off the stack and
-- what values it promises to push.  The WebAssembly standard requires
-- that this type appear explicitly in the code.

data WasmFunctionType pre post =
  WasmFunctionType { ft_pops :: TypeList pre
                   , ft_pushes :: TypeList post
                   }


-- | Representation of WebAssembly control flow.
-- Normally written as
-- @
--   WasmIR pre post
-- @
-- Type parameter `s` is the type of (unspecified) statements.
-- It might be instantiated with an open Cmm block or with a sequence
-- of Wasm instructions.
-- Parameter `e` is the type of expressions.
-- Parameter `pre` represents the values that are expected on the
-- WebAssembly stack when the code runs, and `post` represents
-- the state of the stack on completion.

data WasmIR :: [WasmType] -> [WasmType] -> Type where

  WasmPush  :: WasmTypeTag t -> WasmIR stack (t ': stack) -> WasmIR stack (t ': stack)

  WasmBlock :: WasmFunctionType pre post
            -> WasmIR pre post
            -> WasmIR pre post
  WasmLoop  :: WasmFunctionType pre post
            -> WasmIR pre post
            -> WasmIR pre post
  WasmIfTop :: WasmFunctionType pre post
            -> WasmIR pre post
            -> WasmIR pre post
            -> WasmIR ('I32 ': pre) post

  WasmBr   :: Int -> WasmIR dropped destination -- not typechecked
  WasmFallthrough :: WasmIR dropped destination
          -- generates no code, but has the same type as a branch

  WasmBrTable :: e
              -> BrTableInterval -- for testing
              -> [Int]           -- targets
              -> Int             -- default target
              -> WasmIR dropped destination
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturnTop :: WasmTypeTag t
                -> WasmIR (t ': t1star) t2star -- as per type system

  WasmActions :: s -> WasmIR stack stack   -- basic block: one entry, one exit
  WasmSeq :: WasmIR pre mid -> WasmIR mid post -> WasmIR pre post

  ----------------------------

  WasmConst :: WasmTypeTag t -> Integer -> WasmIR pre (t : pre)

  WasmAdd :: WasmTypeTag t -> WasmIR (t : t : stack) (t : stack)

  WasmCCall :: SymName -> WasmIR pre post -- completely untyped
  WasmGlobalSet :: WasmTypeTag t -> SymName -> WasmIR (t : pre) pre
  WasmLocalGet :: WasmTypeTag t -> Int -> WasmIR pre (t : pre)
  WasmLocalSet :: WasmTypeTag t -> Int -> WasmIR (t : pre) pre



data BrTableInterval
    = BrTableInterval { bti_lo :: Integer, bti_count :: Integer }
  deriving (Show)

instance Outputable BrTableInterval where
  ppr range = brackets $ hcat[integer (bti_lo range), text "..", integer hi]
      where hi = bti_lo range + bti_count range - 1

brTableLimit :: Int
  -- ^ Size of the largest table that is deemed acceptable in a `br_table` instruction.
  --
  -- Source: https://chromium.googlesource.com/v8/v8/+/master/src/wasm/wasm-limits.h#51
  -- See also discussion at https://github.com/WebAssembly/spec/issues/607, which shows
  -- that major browsers agree.
brTableLimit = 65520

inclusiveInterval :: Integer -> Integer -> BrTableInterval
inclusiveInterval lo hi
    | lo <= hi = let count = hi - lo + 1
                 in  if count > toInteger brTableLimit then
                         panic "interval too large in br_table instruction"
                     else
                         BrTableInterval lo count
    | otherwise = panic "GHC.Wasm.ControlFlow: empty interval"

(<>) :: forall pre mid post
      . WasmIR pre mid
     -> WasmIR mid post
     -> WasmIR pre post
(<>) = WasmSeq
-- N.B. Fallthrough can't be optimized away because of type checking.



-- Syntactic sugar.
pattern WasmIf :: WasmFunctionType pre post
               -> WasmIR pre ('I32 : pre)
               -> WasmIR pre post
               -> WasmIR pre post
               -> WasmIR pre post

pattern WasmIf ty e t f =
    WasmPush TagI32 e `WasmSeq` WasmIfTop ty t f

-- More syntactic sugar.
wasmReturn :: WasmTypeTag t -> WasmIR t1star (t : t1star) -> WasmIR t1star t2star
wasmReturn tag e = WasmPush tag e `WasmSeq` WasmReturnTop tag


newtype SymName = SymName FastString

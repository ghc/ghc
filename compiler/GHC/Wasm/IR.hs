{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies, StandaloneKindSignatures, PolyKinds #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for RevAppend
{-# LANGUAGE TypeFamilyDependencies #-}

module GHC.Wasm.IR
  ( WasmIR(..), (<>), pattern WasmIf32, wasmReturn
  , BrTableInterval(..), inclusiveInterval
  , brTableLimit
  , WasmLocal(..)


  , WasmType(..), WasmTypeTag(..)
  , TypeList(..)
  , WasmFunctionType(..)

  , RevAppend, Reverse
  , revappTags, reverseTags


  , SymName(..)

  , WasmCall(..)
  , wasmCallResultTypesReversed
  , wasmCallArgTypesReversed
  )
where

import GHC.Prelude

import Data.Kind
import Data.String
import Data.Type.Equality


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

instance TestEquality WasmTypeTag where
  TagI32 `testEquality` TagI32 = Just Refl
  TagI64 `testEquality` TagI64 = Just Refl
  TagF32 `testEquality` TagF32 = Just Refl
  TagF64 `testEquality` TagF64 = Just Refl
  _ `testEquality` _ = Nothing


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
--   WasmIR bool pre post
-- @
-- Type parameter `bool` represents the Wasm type of Boolean results
-- Parameter `pre` represents the values that are expected on the
-- WebAssembly stack when the code runs, and `post` represents
-- the state of the stack on completion.

newtype WasmLocal = WasmLocal Int


data WasmIR :: WasmType -> [WasmType] -> [WasmType] -> Type where

  WasmPush  :: WasmTypeTag t -> WasmIR bool stack (t ': stack) -> WasmIR bool stack (t ': stack)

  WasmBlock :: WasmFunctionType pre post
            -> WasmIR bool pre post
            -> WasmIR bool pre post
  WasmLoop  :: WasmFunctionType pre post
            -> WasmIR bool pre post
            -> WasmIR bool pre post
  WasmIfTop :: WasmFunctionType pre post
            -> WasmIR bool pre post
            -> WasmIR bool pre post
            -> WasmIR bool (bool ': pre) post

  WasmBr   :: Int -> WasmIR bool dropped destination -- not typechecked
  WasmFallthrough :: WasmIR bool dropped destination
          -- generates no code, but has the same type as a branch

  WasmBrTable :: e
              -> BrTableInterval -- for testing
              -> [Int]           -- targets
              -> Int             -- default target
              -> WasmIR bool dropped destination
              -- invariant: the table interval is contained
              -- within [0 .. pred (length targets)]
  WasmReturnTop :: WasmTypeTag t
                -> WasmIR bool (t ': t1star) t2star -- as per type system

  WasmActions :: s -> WasmIR bool stack stack   -- basic block: one entry, one exit
  WasmSeq :: WasmIR bool pre mid -> WasmIR bool mid post -> WasmIR bool pre post

  ----------------------------

  WasmInt   :: WasmTypeTag t -> Integer  -> WasmIR bool pre (t : pre)
  WasmFloat :: WasmTypeTag t -> Rational -> WasmIR bool pre (t : pre)

  WasmAdd  :: WasmTypeTag t -> WasmIR bool (t : t : stack) (t : stack)
  WasmSub  :: WasmTypeTag t -> WasmIR bool (t : t : stack) (t : stack)
  WasmS_Ge :: WasmTypeTag t -> WasmIR bool (t : t : stack) (bool : stack)  -- signed >=

  WasmNot :: WasmTypeTag t -> WasmIR bool (t : stack) (t : stack)  -- bitwise

  -----

--  WasmCCall :: SymName -> WasmIR bool pre post -- completely untyped
--  WasmGlobalSet :: WasmTypeTag t -> SymName -> WasmIR bool (t : pre) pre
--  WasmLocalGet :: WasmTypeTag t -> Int -> WasmIR bool pre (t : pre)

  WasmSetLocal :: WasmTypeTag t -> WasmLocal -> WasmIR bool (t : pre) pre

  WasmCall :: TypeList argtys
           -> TypeList restys
           -> SymName
           -> WasmIR bool (RevAppend argtys '[]) (RevAppend restys '[])
        -- if '[] is generalized to a variable, then the type fails
        -- the ambiguity check (RevAppend is non-injective)

  WasmCall' :: WasmCall bool pre post -> WasmIR bool pre post



  WasmCallIndirect
      :: WasmTypeTag t -- type of function value on stack
      -> TypeList argtys
      -> TypeList restys
            -- call target hasn't been added yet
      -> WasmIR bool (t : RevAppend argtys '[]) (RevAppend restys '[])

  WasmNop :: WasmIR bool stack stack -- translation of empty list of expressions

data WasmCall :: WasmType -> [WasmType] -> [WasmType] -> Type where
  WasmCallDirect    :: SymName -> WasmCall bool stack stack
  WasmCallAddResult :: WasmTypeTag t
                    -> WasmCall bool pre post
                    -> WasmCall bool pre (t : post)
  WasmCallAddArg    :: WasmTypeTag t
                    -> WasmCall bool pre post
                    -> WasmCall bool (t : pre) post

wasmCallResultTypesReversed :: WasmCall bool pre post
                    -> (forall ts . TypeList ts -> a)
                    -> a
wasmCallResultTypesReversed (WasmCallDirect _) k = k TypeListNil
wasmCallResultTypesReversed (WasmCallAddArg _ call) k = wasmCallResultTypesReversed call k
wasmCallResultTypesReversed (WasmCallAddResult t call) k =
  wasmCallResultTypesReversed call $ \ ts -> k (TypeListCons t ts)

wasmCallArgTypesReversed :: WasmCall bool pre post
                    -> (forall ts . TypeList ts -> a)
                    -> a
wasmCallArgTypesReversed (WasmCallDirect _) k = k TypeListNil
wasmCallArgTypesReversed (WasmCallAddResult _ call) k = wasmCallArgTypesReversed call k
wasmCallArgTypesReversed (WasmCallAddArg t call) k =
  wasmCallArgTypesReversed call $ \ ts -> k (TypeListCons t ts)


data WasmLocals :: [WasmType] -> Type where
  WasmNoLocals :: WasmLocals '[]
  WasmPopLocal :: WasmTypeTag t -> WasmLocal -> WasmLocals ts -> WasmLocals (t : ts)

type RevAppend :: forall a. [a] -> [a] -> [a]
type family RevAppend xs ys where
  RevAppend '[]    ys = ys
  RevAppend (x:xs) ys = RevAppend xs (x : ys)

type Reverse :: forall a . [a] -> [a]
type family Reverse xs where
  Reverse xs = RevAppend xs '[]


revappTags :: TypeList ts -> TypeList us -> TypeList (RevAppend ts us)
revappTags TypeListNil ys = ys
revappTags (TypeListCons t ts) ys = revappTags ts (TypeListCons t ys)

reverseTags :: TypeList ts -> TypeList (Reverse ts)
reverseTags ts = revappTags ts TypeListNil



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

(<>) :: forall bool pre mid post
      . WasmIR bool pre mid
     -> WasmIR bool mid post
     -> WasmIR bool pre post
(<>) = WasmSeq
-- N.B. Fallthrough can't be optimized away because of type checking.



-- Syntactic sugar.
pattern WasmIf32 :: WasmFunctionType pre post
                 -> WasmIR 'I32 pre ('I32 : pre)
                 -> WasmIR 'I32 pre post
                 -> WasmIR 'I32 pre post
                 -> WasmIR 'I32 pre post

pattern WasmIf32 ty e t f =
    WasmPush TagI32 e `WasmSeq` WasmIfTop ty t f

-- More syntactic sugar.
wasmReturn :: WasmTypeTag t -> WasmIR bool t1star (t : t1star) -> WasmIR bool t1star t2star
wasmReturn tag e = WasmPush tag e `WasmSeq` WasmReturnTop tag


newtype SymName = SymName FastString
  deriving (Eq, IsString, Show) via FastString -- , Uniquable
  deriving (Ord) via LexicalFastString

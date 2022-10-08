{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, StandaloneKindSignatures, PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module GHC.Wasm.Tx
  ( expr
  , CG(..)
  , WasmExpr

  , WasmExprs
  , exprs

  , node

  , setLocals
  , WasmLocal(..)

  , WasmAction, WasmTopAction
  )

where

import GHC.Prelude

import Data.String
import Data.Type.Equality

import qualified GHC.Cmm.Type as CT
import GHC.Cmm.CLabel
    import GHC.Cmm.Expr hiding (node)
import GHC.Cmm.Node
import GHC.Platform
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain (assert)
import GHC.Wasm.IR

import GHC.Cmm.Dataflow.Block

----------------------------------------------------------------
---
---     Overview
---
----------------------------------------------------------------
--
-- This module translates Cmm expressions to Wasm instructions while
-- meeting the following goals:
--   * Keep the code compact and readable
--   * Use GADTs to track the stack types of the Wasm code
--   * Check Cmm types only when necessary to prove type correctness
--     of the generated code or when debugging GHC
--



----------------------------------------------------------------
-- code-generation monad

-- This class is a placeholder that expresses the only
-- property used in the prototype: the platform Boolean
-- type is discoverable.


class Monad (codegen bool) => CG bool codegen where
  booleanWasmTypeTag :: codegen bool (WasmTypeTag bool)
  cpsLocalReg :: LocalReg
              -> (forall t . WasmTypeTag t -> WasmLocal -> codegen bool a)
              -> codegen bool a


----------------------------------------------------------------

-- Each Cmm expression is translated into a "WebAssembly expression of
-- type t."  This is Wasm code that can push a value of type `t` onto
-- *any* evaluation stack.

type WasmExpr bool t = (forall stack . WasmIR bool stack (t : stack))


-- At translation time, the target type `t` is not known.
-- It can be defined by a tag, but we wish to avoid the bookkeeping
-- associated with packing a tag and a translation into existentially
-- quantified pair.  Instead, the translator uses continuation-passing
-- style (CPS) to pass a tag and a translation to its continuation.
-- This technique is recommended by Richard Eisenberg, Stitch: The
-- Sound Type-Indexed Type Checker (Functional Pearl), Haskell
-- Symposium 2020 (https://doi.org/10.1145/3406088.3409015).

expr :: CG bool codegen
       => CmmExpr
       -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool a)
       -> codegen bool a
  -- type `a` is the answer type of the continuation

-- The translation is organized as follows:
--
--   * The main translation function `expr` dispatches on the form of
--     a Cmm expression.  (In general, a translation function is named
--     after the thing it translates.)
--
--   * For every different type of Cmm operator, `expr` calls a
--     different auxiliary function: `wasmUnary`, `wasmBinary`,
--     `wasmCompare`, and so on.
--
--     (Since every different type of Cmm operator generates
--     intermediate Wasm code of different types, it seems sensible
--     that each type of operator might require a Haskell translation
--     function of a different type.  But it's a bit irksome.)
--
--   * Each auxiliary function calls back into `expr` to translate
--     operands, if any, then composes the resulting code.
--
--  All functions are CPS.

expr expr k =
  case expr of
    CmmLit (CmmInt n w)   -> wasmNullaryInt   w (flip WasmInt   n) k
    CmmLit (CmmFloat x w) -> wasmNullaryFloat w (flip WasmFloat x) k

    CmmMachOp (MO_Not w) es -> wasmUnary w es WasmNot k

    CmmMachOp (MO_Add w) es -> wasmBinary w es WasmAdd k
    CmmMachOp (MO_Sub w) es -> wasmBinary w es WasmSub k

    CmmMachOp (MO_S_Ge w) es -> wasmCompare w es WasmS_Ge k


    _ -> panic "unimplemented"

------ Types of all the translation functions

-- | Cmm integer and floating-point literals (with zero operands)

wasmNullaryInt, wasmNullaryFloat ::
      CG bool codegen
   => CT.Width
   -> (forall t stack . WasmTypeTag t -> WasmIR bool (stack) (t : stack))
   -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
   -> codegen bool r


-- | Cmm unary operators of type `t -> t`

wasmUnary  :: CG bool codegen
           => CT.Width
           -> [CmmExpr]
           -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : stack) (t : stack))
           -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
           -> codegen bool r


-- | Cmm binary operators of type `t -> t -> t`

wasmBinary ::
    CG bool codegen
 => CT.Width
 -> [CmmExpr]
 -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : t : stack) (t : stack))
 -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
 -> codegen bool r


-- | Cmm binary operators of type `t -> t -> bool`

wasmCompare ::
      forall bool codegen r . CG bool codegen
   => CT.Width
   -> [CmmExpr]
   -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : t : stack) (bool : stack))
   -> (WasmTypeTag bool -> WasmExpr bool bool -> codegen bool r)
   -> codegen bool r

---------- Implementations of the translation functions

wasmNullaryInt w operator k =
  withIntWidthTag w $ \tag -> k tag (operator tag)


wasmNullaryFloat w operator k =
  withFloatWidthTag w $ \tag -> k tag (operator tag)

wasmUnary w [e] operator k =
    expr e $ \tag code -> assert (tag `hasWidth` w) $ k tag (code <> operator tag)
wasmUnary _ _ _ _ = panic "wrong number of operands to unary operator in Cmm"

wasmBinary w es operator k =
    binaryCPS es $ \tag code1 code2 ->
        assert (tag `hasWidth` w) $
        k tag (code1 <> code2 <> operator tag)

wasmCompare w es operator k =
    binaryCPS es $ \tag code1 code2 -> do
      bool <- booleanWasmTypeTag
      assert (bool `hasWidth` w) $
       k bool (code1 <> code2 <> operator tag)

binaryCPS
       :: forall bool codegen a . CG bool codegen
       => [CmmExpr]
       -> (forall t .  WasmTypeTag t
                    -> WasmExpr bool t
                    -> WasmExpr bool t
                    -> codegen bool a)
       -> codegen bool a

binaryCPS [e1, e2] k =   -- would dearly love to use do notation here
    expr e1 $ \tag1 code1 ->
    expr e2 $ \tag2 code2 ->
    case tag1 `testEquality` tag2 of -- mandatory check
      Just Refl -> k tag1 code1 code2
      Nothing -> panic "ill-typed Cmm"
binaryCPS _ _ = panic "wrong number of operands to binary operator in Cmm"

----------------------------------------------------------------


hasWidth :: WasmTypeTag t -> CT.Width -> Bool
hasWidth TagI32 CT.W32 = True
hasWidth TagF32 CT.W32 = True
hasWidth TagI64 CT.W64 = True
hasWidth TagF64 CT.W64 = True
hasWidth _ _ = False


withIntWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withIntWidthTag CT.W32 k = k TagI32
withIntWidthTag CT.W64 k = k TagI64
withIntWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

withFloatWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withFloatWidthTag CT.W32 k = k TagF32
withFloatWidthTag CT.W64 k = k TagF64
withFloatWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"


----------------------------------------------------------------
-- new and experimental

type WasmExprs bool ts = (forall stack . WasmIR bool stack (RevAppend ts stack))

-- | Translate a list of Cmm expressions
exprs :: CG bool codegen
       => [CmmExpr]
       -> (forall ts . TypeList ts -> WasmExprs bool ts -> codegen bool a)
       -> codegen bool a
exprs [] k = k TypeListNil WasmNop
exprs (e:es) k = -- first expression is oldest on stack
  exprs es $ \ts codes ->
    expr e $ \t code ->
      k (TypeListCons t ts) (code <> codes)

type WasmAction bool = (forall stack . WasmIR bool stack stack)
type WasmTopAction bool = (WasmIR bool '[] '[])

-- | Translate an open-open Cmm node (action)

node :: CG bool codegen
     => CmmNode O O
     -> (WasmTopAction bool -> codegen bool a)
           -- using WasmAction here, I can't get the code to typecheck
           -- (non-injectivity of RevAppend)
     -> codegen bool a
node (CmmUnsafeForeignCall (ForeignTarget target _cconv) results arguments) k =
    -- ran out of time to deal with result registers
    exprs arguments $ \argtys arg_codes ->
      localRegs results $ \restys xs ->
        wasmCall argtys restys target $ \code ->
          k (arg_codes <> code <> setLocals restys xs)
node _ _ = panic "more cases needed"

-- | Generate a Wasm call to a Cmm expression
wasmCall :: CG bool codegen
         => TypeList argtypes
         -> TypeList restypes
         -> CmmExpr
         -> (WasmIR bool (Reverse argtypes) (Reverse restypes) -> codegen bool a)
         -> codegen bool a
wasmCall argtypes restypes (CmmLit (CmmLabel callee)) k =
    k (WasmCall argtypes restypes (symNameFromCLabel callee))
wasmCall argtypes restypes e k =
    expr e $ \t code -> k (code <> WasmCallIndirect t argtypes restypes)


-- | set the given local variables from values on the evaluation stack
setLocals :: TypeList ts -> [WasmLocal] -> WasmIR bool (RevAppend ts stack) stack
setLocals TypeListNil [] = WasmNop
setLocals (TypeListCons t ts) (x:xs) = setLocals ts xs <> WasmSetLocal t x
setLocals _ _ = panic "this can't happen -- rewrite code to make it obvious?"


-- | use the CG monad to find the types and locations of local registers
localRegs :: CG bool codegen
      => [LocalReg]
      -> (forall ts . TypeList ts -> [WasmLocal] -> codegen bool a)
      -> codegen bool a
localRegs [] k = k TypeListNil []
localRegs (r:rs) k =
  localRegs rs $ \ts xs ->
    cpsLocalReg r $ \t x ->
      k (TypeListCons t ts) (x:xs)



symNameFromCLabel :: CLabel -> SymName
symNameFromCLabel lbl =
  fromString $
    showSDocOneLine defaultSDocContext {sdocStyle = PprCode AsmStyle} $
      pprCLabel genericPlatform AsmStyle lbl

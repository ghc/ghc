{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.Wasm.Tx
  ( tx
  , CG(..)
  , WasmExpr(..)
  )

where

import GHC.Prelude

import Data.Type.Equality

import qualified GHC.Cmm.Type as CT
import GHC.Cmm.Expr
import GHC.Utils.Panic
import GHC.Wasm.IR

----------------------------------------------------------------

newtype WasmExpr bool t =
    WasmExpr (forall pre . WasmIR bool pre (t : pre))

apply1 :: (forall stack . WasmIR bool (t : stack) (t' : stack))
       -> WasmExpr bool t
       -> WasmExpr bool t'
apply1 operator (WasmExpr code) = WasmExpr (code <> operator)

apply2 :: (forall stack . WasmIR bool (t2 : t1 : stack) (t : stack))
       -> WasmExpr bool t1
       -> WasmExpr bool t2
       -> WasmExpr bool t
apply2 operator (WasmExpr code1) (WasmExpr code2) =
    WasmExpr (code1 <> code2 <> operator)

----------------------------------------------------------------

class Monad (codegen bool) => CG bool codegen where
--  platformWordSize :: m Int
--  asType :: WasmTypeTag t -> m ()  -- check that type is consistent
--                                   -- with platform word size
--  asInt :: WasmTypeTag t -> m ()   -- insist on the platofrm integer type
  booleanWasmTypeTag :: codegen bool (WasmTypeTag bool)



tx :: CG bool codegen
       => CmmExpr
       -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
       -> codegen bool r
  -- combines translation with some type checking

tx expr k =
  case expr of
    CmmLit (CmmInt n w)   -> wasmNullaryInt   w (flip WasmInt   n) k
    CmmLit (CmmFloat x w) -> wasmNullaryFloat w (flip WasmFloat x) k

    CmmMachOp (MO_Not w) es -> wasmUnary w es WasmNot k

    CmmMachOp (MO_Add w) es -> wasmBinary w es WasmAdd k
    CmmMachOp (MO_Sub w) es -> wasmBinary w es WasmSub k

    CmmMachOp (MO_S_Ge w) es -> wasmCompare w es WasmS_Ge k


    _ -> panic "unimplemented"

wasmNullaryInt, wasmNullaryFloat ::
      CG bool codegen
   => CT.Width
   -> (forall t stack . WasmTypeTag t -> WasmIR bool (stack) (t : stack))
   -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
   -> codegen bool r

wasmUnary  :: CG bool codegen
           => CT.Width
           -> [CmmExpr]
           -> (forall t pre . WasmTypeTag t -> WasmIR bool (t : pre) (t : pre))
           -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
           -> codegen bool r

wasmBinary ::
    CG bool codegen
 => CT.Width
 -> [CmmExpr]
 -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : t : stack) (t : stack))
 -> (forall t . WasmTypeTag t -> WasmExpr bool t -> codegen bool r)
 -> codegen bool r


wasmCompare ::
      forall bool codegen r . CG bool codegen
   => CT.Width
   -> [CmmExpr]
   -> (forall t stack . WasmTypeTag t -> WasmIR bool (t : t : stack) (bool : stack))
   -> (WasmTypeTag bool -> WasmExpr bool bool -> codegen bool r)
   -> codegen bool r

----------------------------------------------------------------

wasmNullaryInt w operator k =
  withIntWidthTag w $ \tag -> k tag (WasmExpr $ operator tag)


wasmNullaryFloat w operator k =
  withFloatWidthTag w $ \tag -> k tag (WasmExpr $ operator tag)

wasmUnary w [e] operator k =
    tx e $ \tag code -> checkTagWidth tag w $ k tag (apply1 (operator tag) code)
wasmUnary _ _ _ _ = panic "wrong number of operands to unary operator in Cmm"

wasmBinary w es operator k =
    binaryCPS es $ \tag code1 code2 ->
        checkTagWidth tag w $    -- optional check
        k tag (apply2 (operator tag) code1 code2)

wasmCompare w es operator k =
    binaryCPS es $ \tag code1 code2 -> do
      bool <- booleanWasmTypeTag
      checkTagWidth bool w $
       k bool (apply2 (operator tag) code1 code2)

binaryCPS
       :: forall bool codegen a . CG bool codegen
       => [CmmExpr]
       -> (forall t .  WasmTypeTag t
                    -> WasmExpr bool t
                    -> WasmExpr bool t
                    -> codegen bool a)
       -> codegen bool a

binaryCPS [e1, e2] k =   -- would dearly love to use do notation here
    tx e1 $ \tag1 code1 ->
    tx e2 $ \tag2 code2 ->
    case tag1 `testEquality` tag2 of -- mandatory check
      Just Refl -> k tag1 code1 code2
      Nothing -> panic "ill-typed Cmm"
binaryCPS _ _ = panic "wrong number of operands to binary operator in Cmm"

----------------------------------------------------------------


checkTagWidth :: WasmTypeTag t -> CT.Width -> a -> a
checkTagWidth TagI32 CT.W32 a = a
checkTagWidth TagF32 CT.W32 a = a
checkTagWidth TagI64 CT.W64 a = a
checkTagWidth TagF64 CT.W64 a = a
checkTagWidth _ _ _ = panic "ill-typed Cmm (width mismatch)"


withIntWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withIntWidthTag CT.W32 k = k TagI32
withIntWidthTag CT.W64 k = k TagI64
withIntWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

withFloatWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withFloatWidthTag CT.W32 k = k TagF32
withFloatWidthTag CT.W64 k = k TagF64
withFloatWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

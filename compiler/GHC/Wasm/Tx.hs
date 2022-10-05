{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.Wasm.Tx
  ( tx
  , CG(..)
  )
where

import GHC.Prelude

import Data.Type.Equality
--import Data.Kind

import qualified GHC.Cmm.Type as CT
import GHC.Cmm.Expr
--import GHC.Data.FastString
--import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic
import GHC.Wasm.IR

class Monad (codegen bool) => CG bool codegen where
--  platformWordSize :: m Int
--  asType :: WasmTypeTag t -> m ()  -- check that type is consistent
--                                   -- with platform word size
--  asInt :: WasmTypeTag t -> m ()   -- insist on the platofrm integer type
  booleanWasmTypeTag :: codegen bool (WasmTypeTag bool)

newtype WasmExpr bool t = WasmExpr (forall pre . WarmIR bool pre (t : pre))

tx' :: CG bool codegen
       => CmmExpr
       -> (forall t . WasmTypeTag t -> WasmIR bool pre (t : pre) -> codegen bool r)
       -> codegen bool r

tx :: CG bool codegen
   => CmmExpr
   -> (forall t . WasmTypeTag t -> WasmIR bool '[] (t : '[]) -> codegen bool r)
   -> codegen bool r
 -- combines type checking and translation
tx expr k =
  case expr of
    CmmLit (CmmInt n w)   -> withIntWidthTag   w $ \tag -> k tag (WasmInt tag n)
    CmmLit (CmmFloat x w) -> withFloatWidthTag w $ \tag -> k tag (WasmFloat tag x)

    CmmMachOp (MO_Add w) es -> wasmBinary w es WasmAdd k
    CmmMachOp (MO_Sub w) es -> wasmBinary w es WasmSub k

    CmmMachOp (MO_S_Ge w) es -> wasmCompare w es WasmS_Ge k

    CmmMachOp (MO_Not w) es -> wasmUnary w es WasmNot k

    _ -> panic "unimplemented"

wasmBinary :: CG bool codegen
           => CT.Width
           -> [CmmExpr]
           -> (forall t . WasmTypeTag t -> WasmIR bool (t : t : '[]) (t : '[]))
           -> (forall t . WasmTypeTag t -> WasmIR bool '[] (t : '[]) -> codegen bool r)
           -> codegen bool r

wasmCompare :: forall bool codegen r . CG bool codegen
           => CT.Width
           -> [CmmExpr]
           -> (forall t . WasmTypeTag t -> WasmIR bool (t : t : '[]) (bool : '[]))
           -> (           WasmTypeTag bool -> WasmIR bool '[] (bool : '[]) -> codegen bool r)
           -> codegen bool r

wasmUnary  :: CG bool codegen
           => CT.Width
           -> [CmmExpr]
           -> (forall t . WasmTypeTag t -> WasmIR bool (t : '[]) (t : '[]))
           -> (forall t . WasmTypeTag t -> WasmIR bool '[] (t : '[]) -> codegen bool r)
           -> codegen bool r


wasmBinary w es operator k =
    binaryCPS es $ \tag code1 code2 ->
        checkTagWidth tag w $    -- optional check
        k tag (code1 <> WasmLift tag code2 <> operator tag)


wasmCompare w es operator k =
    binaryCPS es $ \tag code1 code2 -> do
      bool <- booleanWasmTypeTag
      checkTagWidth bool w $
       k bool (code1 <> WasmLift tag code2 <> operator tag)

binaryCPS
       :: forall bool codegen a . CG bool codegen
       => [CmmExpr]
       -> (forall t .  WasmTypeTag t
                    -> WasmIR bool '[] (t : '[])
                    -> WasmIR bool '[] (t : '[])
                    -> codegen bool a)
       -> codegen bool a

binaryCPS [e1, e2] k =   -- would dearly love to use do notation here
    tx e1 $ \tag1 code1 ->
    tx e2 $ \tag2 code2 ->
    case tag1 `testEquality` tag2 of -- mandatory check
      Just Refl -> k tag1 code1 code2
      Nothing -> panic "ill-typed Cmm"
binaryCPS _ _ = panic "wrong number of operands to binary operator in Cmm"

wasmUnary w [e] operator k =
    tx e $ \tag code -> checkTagWidth tag w $ k tag (code <> operator tag)
wasmUnary _ _ _ _ = panic "wrong number of operands to unary operator in Cmm"

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

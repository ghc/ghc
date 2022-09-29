{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Wasm.Tx
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

class Monad m => CG m where
  platformWordSize :: m Int
  asType :: WasmTypeTag t -> m ()  -- check that type is consistent
                                   -- with platform word size
  asInt :: WasmTypeTag t -> m ()   -- insist on the platofrm integer type

tx :: CG m
   => CmmExpr
   -> (forall t . WasmTypeTag t -> WasmIR pre (t : pre) -> m r)
   -> m r
 -- combines type checking and translation
tx expr k =
  case expr of
    CmmLit (CmmInt n w)   -> withIntWidthTag   w $ \tag -> k tag (WasmInt tag n)
    CmmLit (CmmFloat x w) -> withFloatWidthTag w $ \tag -> k tag (WasmFloat tag x)

    CmmMachOp (MO_Add _w) es ->                        -- typecheck _w later
        binary es $ \tag code1 code2 ->
            k tag (code1 <> code2 <> WasmAdd tag)

    _ -> panic "unimplemented"


binary :: CG m
       => [CmmExpr]
       -> (forall t .  WasmTypeTag t
                    -> WasmIR pre (t : pre)
                    -> WasmIR (t : pre) (t : t : pre)
                    -> m a)
       -> m a
binary [e1, e2] k =
    tx e1 $ \tag1 m1 ->
        tx e2 $ \tag2 m2 ->
          case tag1 `testEquality` tag2 of
            Just Refl -> k tag1 (magic m1) (magic m2)
            Nothing -> panic "ill-typed Cmm"

magic :: a -> b
magic x = magic x

whenEqual :: WasmTypeTag t -> WasmTypeTag t' -> (forall t . WasmTypeTag t -> a) -> a
  -- trusted code
whenEqual TagI32 TagI32 k = k TagI32
whenEqual TagF32 TagF32 k = k TagF32
whenEqual TagI64 TagI64 k = k TagI64
whenEqual TagF64 TagF64 k = k TagF64
whenEqual _ _ _ = panic "ill-typed Cmm in Wasm translation"


withIntWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withIntWidthTag CT.W32 k = k TagI32
withIntWidthTag CT.W64 k = k TagI64
withIntWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

withFloatWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withFloatWidthTag CT.W32 k = k TagF32
withFloatWidthTag CT.W64 k = k TagF64
withFloatWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

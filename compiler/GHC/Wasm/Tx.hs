{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.Wasm.Tx
where

import GHC.Prelude

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
    CmmLit (CmmInt n w) -> withIntWidthTag w $ \tag -> k tag (WasmConst tag n)
    _ -> panic "unimplemented"



withIntWidthTag :: CT.Width -> (forall t . WasmTypeTag t -> a) -> a
withIntWidthTag CT.W32 k = k TagI32
withIntWidthTag CT.W64 k = k TagI64
withIntWidthTag w _ = panic $ "width " ++ show w ++ " not supported on wasm target"

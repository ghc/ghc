{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module GHC.CmmToAsm.Wasm (ncgWasm) where

import Data.ByteString.Builder
import Data.Maybe
import Data.Semigroup
import GHC.Cmm
import GHC.CmmToAsm.Wasm.Asm
import GHC.CmmToAsm.Wasm.FromCmm
import GHC.CmmToAsm.Wasm.Types
import GHC.Data.Stream (Stream, StreamS (..), runStream)
import GHC.Platform
import GHC.Prelude
import GHC.Types.Unique.Supply
import GHC.Unit
import System.IO

ncgWasm ::
  Platform ->
  UniqSupply ->
  ModLocation ->
  Handle ->
  Stream IO RawCmmGroup a ->
  IO a
ncgWasm platform us loc h cmms = do
  (r, s) <- streamCmmGroups platform us cmms
  hPutBuilder h $ "# " <> string7 (fromJust $ ml_hs_file loc) <> "\n\n"
  hPutBuilder h $ execWasmAsmM $ asmTellEverything TagI32 s
  pure r

streamCmmGroups ::
  Platform ->
  UniqSupply ->
  Stream IO RawCmmGroup a ->
  IO (a, WasmCodeGenState 'I32)
streamCmmGroups platform us cmms =
  go (initialWasmCodeGenState platform us) $
    runStream cmms
  where
    go s (Done r) = pure (r, s)
    go s (Effect m) = m >>= go s
    go s (Yield cmm k) = go (wasmExecM (onCmmGroup cmm) s) k

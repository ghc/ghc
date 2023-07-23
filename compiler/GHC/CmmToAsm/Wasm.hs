{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module GHC.CmmToAsm.Wasm (ncgWasm) where

import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe
import Data.Semigroup
import GHC.Cmm
import GHC.CmmToAsm.Wasm.Asm
import GHC.CmmToAsm.Wasm.FromCmm
import GHC.CmmToAsm.Wasm.Types
import GHC.Data.Stream (Stream, StreamS (..), runStream)
import GHC.Driver.DynFlags
import GHC.Platform
import GHC.Prelude
import GHC.Settings
import GHC.Types.Unique.Supply
import GHC.Unit
import GHC.Utils.Logger
import GHC.Utils.Outputable (text)
import System.IO

ncgWasm ::
  Logger ->
  Platform ->
  ToolSettings ->
  UniqSupply ->
  ModLocation ->
  Handle ->
  Stream IO RawCmmGroup a ->
  IO a
ncgWasm logger platform ts us loc h cmms = do
  (r, s) <- streamCmmGroups platform us cmms
  outputWasm $ "# " <> string7 (fromJust $ ml_hs_file loc) <> "\n\n"
  outputWasm $ execWasmAsmM do_tail_call $ asmTellEverything TagI32 s
  pure r
  where
    -- See Note [WasmTailCall]
    do_tail_call = doTailCall ts

    outputWasm builder = do
      putDumpFileMaybe
        logger
        Opt_D_dump_asm
        "Asm Code"
        FormatASM
        (text . unpack $ toLazyByteString builder)
      hPutBuilder h builder

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

doTailCall :: ToolSettings -> Bool
doTailCall ts = Option "-mtail-call" `elem` as_args
  where
    (_, as_args) = toolSettings_pgm_a ts

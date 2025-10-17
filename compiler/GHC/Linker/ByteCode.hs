-- Utilities for creating bytecode libraries
module GHC.Linker.ByteCode where

import GHC.Prelude
import GHC.ByteCode.Serialize
import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Driver.Env
import GHC.Utils.Outputable
import GHC.Linker.Loader
import qualified Data.ByteString as BS


linkBytecodeLib :: HscEnv -> [ModuleByteCode] -> IO ()
linkBytecodeLib hsc_env gbcs = do
  let dflags = hsc_dflags hsc_env
  -- The .gbc files from the command line
  let bytecodeObjects = [f | FileOption _ f <- ldInputs dflags]

  let logger = hsc_logger hsc_env
  let allFiles = (map text bytecodeObjects) ++ [ angleBrackets (text "in-memory" <+>  ppr (gbc_module bco)) | bco <- gbcs ]
  debugTraceMsg logger 2 $
    text "linkBytecodeLib: linking the following bytecode objects:" $$
    vcat allFiles


  on_disk_bcos <- mapM (readBinByteCode hsc_env) bytecodeObjects

  let (all_cbcs, foreign_stubs) = unzip [ (bs, fs) | ModuleByteCode _m bs fs <- on_disk_bcos ++ gbcs]

  foreign_stub_lib <- mkDynLoadLib hsc_env id [] [{-TODO-}] (concat foreign_stubs)

  fc <- case foreign_stub_lib of
    Just (fp, _, _) -> do
      file <- BS.readFile fp
      pure (Just (SharedObjectContents file))
    Nothing -> pure Nothing

  let bytecodeLib' = BytecodeLib {
    bytecodeLibUnitId = homeUnitId_ dflags,
    bytecodeLibFiles = all_cbcs,
    bytecodeLibForeign = fc
  }
  let output_fn = case outputFile dflags of { Just s -> s; Nothing -> "a.out"; }
  writeBytecodeLib bytecodeLib' output_fn
  return ()


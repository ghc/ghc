-- Utilities for creating bytecode libraries
module GHC.Linker.ByteCode where

import GHC.Prelude
import GHC.ByteCode.Serialize
import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Driver.Env
import GHC.Utils.Outputable


linkBytecodeLib :: HscEnv -> [ByteCodeObject] -> IO ()
linkBytecodeLib hsc_env gbcs = do
  let dflags = hsc_dflags hsc_env
  -- The .gbc files from the command line
  let bytecodeObjects = [f | FileOption _ f <- ldInputs dflags]

  let logger = hsc_logger hsc_env
  let allFiles = (map text bytecodeObjects) ++ [ angleBrackets (text "in-memory" <+>  ppr (bco_module bco)) | bco <- gbcs ]
  debugTraceMsg logger 2 $
    text "linkBytecodeLib: linking the following bytecode objects:" $$
    vcat allFiles

  bytecodeLib <- mkBytecodeLib hsc_env bytecodeObjects gbcs
  let output_fn = case outputFile dflags of { Just s -> s; Nothing -> "a.out"; }
  writeBytecodeLib bytecodeLib output_fn
  return ()


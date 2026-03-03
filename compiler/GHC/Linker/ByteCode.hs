-- Utilities for creating bytecode libraries
module GHC.Linker.ByteCode where

import GHC.Prelude
import GHC.ByteCode.Serialize
import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Driver.Env
import GHC.Utils.Outputable
import GHC.Linker.Loader
import Data.List (partition)
import GHC.Driver.Phases (isBytecodeFilename)
import GHC.Runtime.Interpreter (interpreterDynamic)
import Data.Maybe


linkBytecodeLib :: HscEnv -> [ModuleByteCode] -> IO ()
linkBytecodeLib hsc_env gbcs = do
  let dflags = hsc_dflags hsc_env
  -- The .gbc files from the command line
  let fileArguments = [f | FileOption _ f <- ldInputs dflags]

  let (bytecodeObjects, objectFiles) = partition isBytecodeFilename fileArguments

  let logger = hsc_logger hsc_env
  let allFiles = (map text bytecodeObjects) ++ [ angleBrackets (text "in-memory" <+>  ppr (gbc_module bco)) | bco <- gbcs ]
  debugTraceMsg logger 2 $
    text "linkBytecodeLib: linking the following bytecode objects:" $$
    vcat allFiles


  on_disk_bcos <- mapM (readBinByteCode hsc_env) bytecodeObjects

  let (all_cbcs, foreign_stubs) = unzip [ (bs, fs) | ModuleByteCode _m bs fs _hash <- on_disk_bcos ++ gbcs]

  interpreter_foreign_lib <- mkInterpreterLib hsc_env (concat foreign_stubs ++ objectFiles)

  let bytecodeLib' = BytecodeLib {
    bytecodeLibUnitId = homeUnitId_ dflags,
    bytecodeLibFiles = all_cbcs,
    bytecodeLibForeign = interpreter_foreign_lib
  }
  let output_fn = fromMaybe "a.out" (outputFile dflags)
  writeBytecodeLib bytecodeLib' output_fn
  return ()


-- | Build a library suitable for loading into the interpreter.
--
-- This uses similar logic to how foreign stubs are compiler specifically for
-- a specific interpeter way. If the interpreter is dynamic, we create a shared library
-- and if it's static, create a static archive.
--
-- The objects which we use will already be compiled for this scheme.
-- It doesn't appear exactly right to use interpreterDynamic here, but it's what
-- is currently done for foreign stubs in GHC.Driver.ByteCode.
-- Perhaps instead we should look at the build way to determine which kind of library to create.
mkInterpreterLib :: HscEnv -> [FilePath] -> IO (Maybe InterpreterLibrary)
mkInterpreterLib hsc_env files =
  case interpreterDynamic (hscInterp hsc_env) of
    True -> do
      -- I'm not sure that mkDynLoadLib is exactly the right way to create a shared library for the foreign stubs,
      -- It is something we can improve later based on feedback from users.
      foreign_stub_lib <- mkDynLoadLib hsc_env id [] [] files
      case foreign_stub_lib of
        Just (foreign_stub_lib_path, foreign_stub_lib_dir, foreign_stub_lib_name) -> do
          return $ Just (InterpreterSharedObject foreign_stub_lib_path foreign_stub_lib_dir foreign_stub_lib_name)
        Nothing -> pure Nothing
    False -> do
      pure $ Just (InterpreterStaticObjects files)

module GHC.Driver.ByteCode where


import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.CodeOutput
import GHC.Driver.Env
import GHC.Runtime.Interpreter
import GHC.Tc.Utils.Monad

import GHC.Unit
import GHC.Types.ForeignStubs
import GHC.Data.Maybe

import {-# SOURCE #-} GHC.Driver.Pipeline

import GHC.Platform.Ways


-- | Write foreign sources and foreign stubs to temporary files and compile them.
outputAndCompileForeign :: HscEnv -> Module -> ModLocation -> [(ForeignSrcLang, FilePath)] ->  ForeignStubs -> IO [FilePath]
outputAndCompileForeign hsc_env mod_name location foreign_files foreign_stubs = do
  let dflags   = hsc_dflags hsc_env
      logger   = hsc_logger hsc_env
      tmpfs    = hsc_tmpfs hsc_env
  (_, has_stub) <- outputForeignStubs logger tmpfs dflags (hsc_units hsc_env) mod_name location foreign_stubs
  compile_for_interpreter hsc_env $ \ i_env -> do
    stub_o <- traverse (compileForeign i_env LangC) has_stub
    foreign_files_o <- traverse (uncurry (compileForeign i_env)) foreign_files
    pure (maybeToList stub_o ++ foreign_files_o)

-- | Modify flags such that objects are compiled for the interpreter's way.
-- This is necessary when building foreign objects for Template Haskell, since
-- those are object code built outside of the pipeline, which means they aren't
-- subject to the mechanism in 'enableCodeGenWhen' that requests dynamic build
-- outputs for dependencies when the interpreter used for TH is dynamic but the
-- main outputs aren't.
-- Furthermore, the HPT only stores one set of objects with different names for
-- bytecode linking in 'HomeModLinkable', so the usual hack for switching
-- between ways in 'get_link_deps' doesn't work.
compile_for_interpreter :: HscEnv -> (HscEnv -> IO a) -> IO a
compile_for_interpreter hsc_env use =
  use (hscUpdateFlags update hsc_env)
  where
    update dflags = dflags {
      targetWays_ = adapt_way interpreterDynamic WayDyn $
                    adapt_way interpreterProfiled WayProf $
                    targetWays_ dflags
      }

    adapt_way want = if want (hscInterp hsc_env) then addWay else removeWay

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Ld
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ld@ linker program.

module Distribution.Simple.Program.Ld (
    combineObjectFiles,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Compiler (arResponseFilesSupported)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.ResponseFile
         ( withResponseFile )
import Distribution.Simple.Program.Run
         ( ProgramInvocation, programInvocation, multiStageProgramInvocation
         , runProgramInvocation )
import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..) )
import Distribution.Simple.Setup
         ( fromFlagOrDefault, configUseResponseFiles )
import Distribution.Simple.Utils
         ( defaultTempFileOptions )
import Distribution.Verbosity
         ( Verbosity )

import System.Directory
         ( renameFile )
import System.FilePath
         ( (<.>), takeDirectory )

-- | Call @ld -r@ to link a bunch of object files together.
--
combineObjectFiles :: Verbosity -> LocalBuildInfo -> ConfiguredProgram
                   -> FilePath -> [FilePath] -> IO ()
combineObjectFiles verbosity lbi ld target files = do

  -- Unlike "ar", the "ld" tool is not designed to be used with xargs. That is,
  -- if we have more object files than fit on a single command line then we
  -- have a slight problem. What we have to do is link files in batches into
  -- a temp object file and then include that one in the next batch.

  let simpleArgs  = ["-r", "-o", target]

      initialArgs = ["-r", "-o", target]
      middleArgs  = ["-r", "-o", target, tmpfile]
      finalArgs   = middleArgs

      simple      = programInvocation ld simpleArgs
      initial     = programInvocation ld initialArgs
      middle      = programInvocation ld middleArgs
      final       = programInvocation ld finalArgs

      targetDir   = takeDirectory target

      invokeWithResponesFile :: FilePath -> ProgramInvocation
      invokeWithResponesFile atFile =
        programInvocation ld $ simpleArgs ++ ['@' : atFile]

      oldVersionManualOverride =
        fromFlagOrDefault False $ configUseResponseFiles $ configFlags lbi
      -- Whether ghc's ar supports response files is a good proxy for
      -- whether ghc's ld supports them as well.
      responseArgumentsNotSupported   =
        not (arResponseFilesSupported (compiler lbi))

  if oldVersionManualOverride || responseArgumentsNotSupported
    then
      run $ multiStageProgramInvocation simple (initial, middle, final) files
    else
      withResponseFile verbosity defaultTempFileOptions targetDir "ld.rsp" Nothing files $
        \path -> runProgramInvocation verbosity $ invokeWithResponesFile path

  where
    tmpfile        = target <.> "tmp" -- perhaps should use a proper temp file

    run :: [ProgramInvocation] -> IO ()
    run []         = return ()
    run [inv]      = runProgramInvocation verbosity inv
    run (inv:invs) = do runProgramInvocation verbosity inv
                        renameFile target tmpfile
                        run invs


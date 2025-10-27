-- | External ("system") linker
module GHC.Linker.External
  ( LinkerConfig(..)
  , runLink
  )
where

import GHC.Prelude
import GHC.Utils.TmpFs
import GHC.Utils.Logger
import GHC.Utils.Error
import GHC.Utils.CliOption
import GHC.SysTools.Process
import GHC.Linker.Config

-- | Run the external linker
runLink :: Logger -> TmpFs -> LinkerConfig -> Bool -> [Option] -> IO ()
runLink logger tmpfs cfg require_cxx args = traceSystoolCommand logger "linker" $ do
  let all_args = linkerOptionsPre cfg ++ args ++ linkerOptionsPost cfg

  -- on Windows, mangle environment variables to account for a bug in Windows
  -- Vista
  mb_env <- getGccEnv all_args
 
  -- sneakily switch to C++ compiler when we need C++ standard lib
  let prog
        | require_cxx = linkerCXX cfg
        | otherwise   = linkerC cfg
  -- FIXME: ld flags may be totally inappropriate for the C++ compiler?

  runSomethingResponseFile logger tmpfs (linkerTempDir cfg) (linkerFilter cfg)
    "Linker" prog all_args mb_env

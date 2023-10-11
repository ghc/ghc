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
runLink :: Logger -> TmpFs -> LinkerConfig -> [Option] -> IO ()
runLink logger tmpfs cfg args = traceSystoolCommand logger "linker" $ do
  let all_args = linkerOptionsPre cfg ++ args ++ linkerOptionsPost cfg

  -- on Windows, mangle environment variables to account for a bug in Windows
  -- Vista
  mb_env <- getGccEnv all_args

  runSomethingResponseFile logger tmpfs (linkerTempDir cfg) (linkerFilter cfg)
    "Linker" (linkerProgram cfg) all_args mb_env

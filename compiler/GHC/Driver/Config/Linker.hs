module GHC.Driver.Config.Linker
  ( initFrameworkOpts
  ) where

import GHC.Linker.Config

import GHC.Driver.Session

initFrameworkOpts :: DynFlags -> FrameworkOpts
initFrameworkOpts dflags = FrameworkOpts
  { foFrameworkPaths    = frameworkPaths    dflags
  , foCmdlineFrameworks = cmdlineFrameworks dflags
  }

-- | Linker configuration

module GHC.Linker.Config
  ( FrameworkOpts(..)
  , LinkerConfig(..)
  )
where

import GHC.Prelude
import GHC.Utils.TmpFs
import GHC.Utils.CliOption

-- used on darwin only
data FrameworkOpts = FrameworkOpts
  { foFrameworkPaths    :: [String]
  , foCmdlineFrameworks :: [String]
  }

-- | External linker configuration
data LinkerConfig = LinkerConfig
  { linkerProgram     :: String           -- ^ Linker program
  , linkerOptionsPre  :: [Option]         -- ^ Linker options (before user options)
  , linkerOptionsPost :: [Option]         -- ^ Linker options (after user options)
  , linkerTempDir     :: TempDir          -- ^ Temporary directory to use
  , linkerFilter      :: [String] -> [String] -- ^ Output filter
  }


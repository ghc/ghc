{-# LANGUAGE CPP #-}
-- |
-- Stability: unstable
-- Portability: unportable
--
-- Internal modules are always subject to change from version to version.
-- The contents of this module are also platform-dependent, hence what is
-- shown in the Hackage documentation may differ from what is actually
-- available on your system.

#include <HsDirectoryConfig.h>

module System.Directory.Internal
  ( module System.Directory.Internal.Common

#ifdef mingw32_HOST_OS
  , module System.Directory.Internal.Windows
#else
  , module System.Directory.Internal.Posix
#endif

#ifdef HAVE_UTIMENSAT
  , module System.Directory.Internal.C_utimensat
#endif

  ) where

import System.Directory.Internal.Common

#ifdef mingw32_HOST_OS
import System.Directory.Internal.Windows
#else
import System.Directory.Internal.Posix
#endif

#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif

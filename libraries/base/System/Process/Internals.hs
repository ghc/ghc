{-# OPTIONS_GHC -cpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Internals
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

-- #hide
module System.Process.Internals (
	ProcessHandle(..), PHANDLE,
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
	 pPrPr_disableITimers, c_execvpe
#endif
  ) where

import Prelude -- necessary to get dependencies right

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Types ( CPid )
#else
import Data.Word ( Word32 )
#endif
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt )
import Foreign.Ptr ( Ptr )

#ifdef __HUGS__
{-# CFILES cbits/execvpe.c  #-}
#endif

-- ----------------------------------------------------------------------------
-- ProcessHandle type

{- | A handle to a process, which can be used to wait for termination
     of the process using 'waitForProcess'.

     None of the process-creation functions in this library wait for
     termination: they all return a 'ProcessHandle' which may be used
     to wait for the process later.
-}
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
type PHANDLE = CPid
#else
type PHANDLE = Word32
#endif

newtype ProcessHandle = ProcessHandle PHANDLE

-- ----------------------------------------------------------------------------

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

-- this function disables the itimer, which would otherwise cause confusing
-- signals to be sent to the new process.
foreign import ccall unsafe "pPrPr_disableITimers"
  pPrPr_disableITimers :: IO ()

foreign import ccall unsafe "execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt

#endif

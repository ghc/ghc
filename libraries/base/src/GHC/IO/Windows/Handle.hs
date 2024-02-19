-- |
-- Module      :  GHC.IO.Windows.Handle
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on Windows Handles
--

module GHC.IO.Windows.Handle
 ( -- * Basic Types
   NativeHandle(),
   ConsoleHandle(),
   IoHandle(),
   HANDLE,
   Io(),

   -- * Utility functions
   convertHandle,
   toHANDLE,
   fromHANDLE,
   handleToMode,
   isAsynchronous,
   optimizeFileAccess,

   -- * Standard Handles
   stdin,
   stdout,
   stderr,

   -- * File utilities
   openFile,
   openFileAsTemp,
   release
 ) where

import GHC.Internal.IO.Windows.Handle

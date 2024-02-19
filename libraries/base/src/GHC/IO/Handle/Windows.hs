-- |
-- Module      :  GHC.IO.Handle.Windows
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Handle operations implemented by Windows native handles
--
-----------------------------------------------------------------------------

module GHC.IO.Handle.Windows (
  stdin, stdout, stderr,
  openFile, openBinaryFile, openFileBlocking,
  handleToHANDLE, mkHandleFromHANDLE
 ) where

import GHC.Internal.IO.Handle.Windows

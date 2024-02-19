{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.BufferedIO
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Class of buffered IO devices
--

module GHC.IO.BufferedIO
    (BufferedIO(..),
     readBuf,
     readBufNonBlocking,
     writeBuf,
     writeBufNonBlocking
     ) where

import GHC.Internal.IO.BufferedIO
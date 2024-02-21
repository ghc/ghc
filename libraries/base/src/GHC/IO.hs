{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.IO
-- Copyright   :  (c) The University of Glasgow 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Definitions for the 'IO' monad and its friends.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.IO (
        IO(..), unIO, liftIO, mplusIO,
        unsafePerformIO, unsafeInterleaveIO,
        unsafeDupablePerformIO, unsafeDupableInterleaveIO,
        noDuplicate,

        -- To and from ST
        stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        FilePath,

        catch, catchException, catchAny, throwIO,
        mask, mask_, uninterruptibleMask, uninterruptibleMask_,
        MaskingState(..), getMaskingState,
        unsafeUnmask, interruptible,
        onException, bracket, finally, evaluate,
        mkUserError
    ) where

import GHC.Internal.IO

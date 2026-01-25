{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.IO.Handle.Text
-- Copyright   :  (c) The University of Glasgow, 1992-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable
--
-- String I\/O functions
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.IO.Handle.Text should be removed in GHC 10.02."
#endif

module GHC.IO.Handle.Text (
  {-# DEPRECATED ["GHC.IO.Handle.Text is deprecated and will be removed in GHC 10.02. Please use the ghc-internal package."] #-}
        hWaitForInput, hGetChar, hGetLine, hGetContents, hPutChar, hPutStr,
        commitBuffer',       -- hack, see below
        hGetBuf, hGetBufSome, hGetBufNonBlocking, hPutBuf, hPutBufNonBlocking,
        memcpy, hPutStrLn, hGetContents',
    ) where

import GHC.Internal.IO.Handle.Text

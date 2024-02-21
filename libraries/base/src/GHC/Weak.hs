{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Weak
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Weak pointers.
--

module GHC.Weak
    (Weak(..),
     mkWeak,
     deRefWeak,
     finalize,
     -- *  Handling exceptions
     -- |  When an exception is thrown by a finalizer called by the
     -- garbage collector, GHC calls a global handler which can be set with
     -- 'setFinalizerExceptionHandler'. Note that any exceptions thrown by
     -- this handler will be ignored.
     setFinalizerExceptionHandler,
     getFinalizerExceptionHandler,
     printToHandleFinalizerExceptionHandler
     ) where

import GHC.Internal.Weak

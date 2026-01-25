{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Exception.Type
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.Exception.Type should be removed in GHC 10.02."
#endif

module GHC.Exception.Type
  {-# DEPRECATED ["GHC.Exception.Type is deprecated and will be removed in GHC 10.02. Please use Control.Exception where possible and the ghc-internal package otherwise."] #-}
       ( Exception(..)    -- Class
       , SomeException(..), ArithException(..)
       , divZeroException, overflowException, ratioZeroDenomException
       , underflowException
       ) where

import GHC.Internal.Exception.Type

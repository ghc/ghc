-- |
-- Module      :  Control.Exception.Context
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exception context and annotations.
--
module Control.Exception.Context
    ( ExceptionContext(..)
    , emptyExceptionContext
    , addExceptionAnnotation
      -- * Destructuring
    , getExceptionAnnotations
    , getAllExceptionAnnotations
    , displayExceptionContext
    ) where

import GHC.Internal.Exception.Context

-- |
-- Module      :  Control.Exception.Annotation
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exception annotations.
--
module Control.Exception.Annotation
    ( SomeExceptionAnnotation(..)
    , ExceptionAnnotation(..)
    ) where

import GHC.Internal.Exception.Context


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception.Context
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exception context type.
--
-----------------------------------------------------------------------------

module GHC.Exception.Context
    ( -- * Exception context
      ExceptionContext(..)
    , emptyExceptionContext
    , addExceptionAnnotation
    , getExceptionAnnotations
    , getAllExceptionAnnotations
    , mergeExceptionContext
      -- * Exception annotations
    , SomeExceptionAnnotation(..)
    , ExceptionAnnotation(..)
    ) where

import GHC.Base ((++), return, String, Maybe(..), Semigroup(..), Monoid(..))
import GHC.Show (Show(..))
import Data.Typeable.Internal (Typeable, typeRep, eqTypeRep)
import Data.Type.Equality ( (:~~:)(HRefl) )

-- | Exception context represents a list of 'ExceptionAnnotation's. These are
-- attached to 'SomeException's via 'Control.Exception.addExceptionContext' and
-- can be used to capture various ad-hoc metadata about the exception including
-- backtraces and application-specific context.
--
-- 'ExceptionContext's can be merged via concatenation using the 'Semigroup'
-- instance or 'mergeExceptionContext'.
data ExceptionContext = ExceptionContext [SomeExceptionAnnotation]

instance Semigroup ExceptionContext where
    (<>) = mergeExceptionContext

instance Monoid ExceptionContext where
    mempty = emptyExceptionContext

-- | An 'ExceptionContext' containing no annotations.
emptyExceptionContext :: ExceptionContext
emptyExceptionContext = ExceptionContext []

-- | Construct a singleton 'ExceptionContext' from an 'ExceptionAnnotation'.
addExceptionAnnotation :: ExceptionAnnotation a => a -> ExceptionContext -> ExceptionContext
addExceptionAnnotation x (ExceptionContext xs) = ExceptionContext (SomeExceptionAnnotation x : xs)

getExceptionAnnotations :: forall a. ExceptionAnnotation a => ExceptionContext -> [a]
getExceptionAnnotations (ExceptionContext xs) =
    [ x
    | SomeExceptionAnnotation (x :: b) <- xs 
    , Just HRefl <- return (typeRep @a `eqTypeRep` typeRep @b)
    ]

getAllExceptionAnnotations :: ExceptionContext -> [SomeExceptionAnnotation]
getAllExceptionAnnotations (ExceptionContext xs) = xs

-- | Merge two 'ExceptionContext's via concatenation
mergeExceptionContext :: ExceptionContext -> ExceptionContext -> ExceptionContext
mergeExceptionContext (ExceptionContext a) (ExceptionContext b) = ExceptionContext (a ++ b)


data SomeExceptionAnnotation = forall a. ExceptionAnnotation a => SomeExceptionAnnotation a

-- | 'ExceptionAnnotation's are types which can decorate exceptions as
-- 'ExceptionContext'.
class (Typeable a) => ExceptionAnnotation a where
    -- | Render the annotation for display to the user.
    displayExceptionAnnotation :: a -> String

    default displayExceptionAnnotation :: Show a => a -> String
    displayExceptionAnnotation = show


{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , DataKinds
           , ExistentialQuantification
           , RankNTypes
           , StandaloneKindSignatures
           , TypeFamilies
           , TypeFamilyDependencies
  #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Desugar
-- Copyright   :  (c) The University of Glasgow, 2007
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Support code for desugaring in GHC
--
-----------------------------------------------------------------------------

module GHC.Desugar
  ( -- * Arrows
    (>>>), ArrowEnv(..), ArrowStackTup, ArrowEnvTup
    -- * Annotations
  , AnnotationWrapper(..), toAnnotationWrapper
  ) where

import Control.Arrow    (Arrow(..))
import Control.Category ((.))
import Data.Data        (Data)
import Data.Kind        (Type)

-- A version of Control.Category.>>> overloaded on Arrow
(>>>) :: forall arr. Arrow arr => forall a b c. arr a b -> arr b c -> arr a c
-- NB: the type of this function is the "shape" that GHC expects
--     in tcInstClassOp.  So don't put all the foralls at the front!
--     Yes, this is a bit grotesque, but heck it works and the whole
--     arrows stuff needs reworking anyway!
f >>> g = g . f
{-# INLINE (>>>) #-} -- see Note [INLINE on >>>] in Control.Category

newtype ArrowEnv a = ArrowEnv a

type ArrowStackTup :: [Type] -> Type
type family ArrowStackTup stk

type ArrowEnvTup :: Type -> [Type] -> Type
type family ArrowEnvTup env stk = arg | arg -> env stk

-- A wrapper data type that lets the typechecker get at the appropriate dictionaries for an annotation
data AnnotationWrapper = forall a. (Data a) => AnnotationWrapper a

toAnnotationWrapper :: (Data a) => a -> AnnotationWrapper
toAnnotationWrapper what = AnnotationWrapper what

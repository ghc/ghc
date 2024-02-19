{-# LANGUAGE NoImplicitPrelude
           , RankNTypes
           , ExistentialQuantification
           , Safe
  #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Desugar
-- Copyright   :  (c) The University of Glasgow, 2007
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Support code for desugaring in GHC
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.Desugar ((>>>), AnnotationWrapper(..), toAnnotationWrapper) where

import GHC.Internal.Control.Arrow    (Arrow(..))
import GHC.Internal.Control.Category ((.))
import GHC.Internal.Data.Data        (Data)

-- A version of Control.Category.>>> overloaded on Arrow
(>>>) :: forall arr. Arrow arr => forall a b c. arr a b -> arr b c -> arr a c
-- NB: the type of this function is the "shape" that GHC expects
--     in tcInstClassOp.  So don't put all the foralls at the front!
--     Yes, this is a bit grotesque, but heck it works and the whole
--     arrows stuff needs reworking anyway!
f >>> g = g . f
{-# INLINE (>>>) #-} -- see Note [INLINE on >>>] in GHC.Internal.Control.Category

-- A wrapper data type that lets the typechecker get at the appropriate dictionaries for an annotation
data AnnotationWrapper = forall a. (Data a) => AnnotationWrapper a

toAnnotationWrapper :: (Data a) => a -> AnnotationWrapper
toAnnotationWrapper what = AnnotationWrapper what

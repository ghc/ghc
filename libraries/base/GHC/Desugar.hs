{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , RankNTypes
           , ExistentialQuantification
  #-}

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

-- #hide
module GHC.Desugar ((>>>), AnnotationWrapper(..), toAnnotationWrapper) where

import Control.Arrow    (Arrow(..))
import Control.Category ((.))
import Data.Data        (Data)

-- A version of Control.Category.>>> overloaded on Arrow
(>>>) :: forall arr. Arrow arr => forall a b c. arr a b -> arr b c -> arr a c
-- NB: the type of this function is the "shape" that GHC expects
--     in tcInstClassOp.  So don't put all the foralls at the front!  
--     Yes, this is a bit grotesque, but heck it works and the whole
--     arrows stuff needs reworking anyway!
f >>> g = g . f

-- A wrapper data type that lets the typechecker get at the appropriate dictionaries for an annotation
data AnnotationWrapper = forall a. (Data a) => AnnotationWrapper a

toAnnotationWrapper :: (Data a) => a -> AnnotationWrapper
toAnnotationWrapper what = AnnotationWrapper what


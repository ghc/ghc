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
module GHC.Desugar ((>>>)) where

import Control.Arrow    (Arrow(..))
import Control.Category ((.))
import Prelude hiding ((.))

-- A version of Control.Category.>>> overloaded on Arrow
#ifndef __HADDOCK__
(>>>) :: forall arr. Arrow arr => forall a b c. arr a b -> arr b c -> arr a c
#endif
-- NB: the type of this function is the "shape" that GHC expects
--     in tcInstClassOp.  So don't put all the foralls at the front!  
--     Yes, this is a bit grotesque, but heck it works and the whole
--     arrows stuff needs reworking anyway!
f >>> g = g . f

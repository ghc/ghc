-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC Extensions: this is the Approved Way to get at GHC-specific stuff.
--
-----------------------------------------------------------------------------

module GHC.Exts
       (
        -- the representation of some basic types:
        Int(..),Word(..),Float(..),Double(..),Integer(..),Char(..),

	-- Fusion
	build, augment,

	-- shifty wrappers from GHC.Base
	shiftL#, shiftRL#, iShiftL#, iShiftRA#, iShiftRL#,

	-- for linear implicit parameters:
	Splittable(..),

        -- and finally, all the unboxed primops of GHC!
        module GHC.Prim

       ) where

import Prelude

import GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Num
import GHC.Float

class Splittable t where
  split :: t -> (t,t)

-----------------------------------------------------------------------------
-- 
-- Module      :  GHC.Exts
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Exts.hs,v 1.3 2002/03/14 16:26:40 simonmar Exp $
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

import {-# SOURCE #-} GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Num
import GHC.Float

class Splittable t where
  split :: t -> (t,t)

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
-- $Id: Exts.hs,v 1.2 2002/01/02 15:01:27 simonmar Exp $
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

        -- and finally, all the unboxed primops of GHC!
        module GHC.Prim

       ) where

import Prelude

import {-# SOURCE #-} GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Num
import GHC.Float

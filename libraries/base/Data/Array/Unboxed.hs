-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Array.Unboxed
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Unboxed.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Unboxed immutable array type.
--
-----------------------------------------------------------------------------

module Data.Array.Unboxed (
   module Data.Array.IArray,
   UArray,
 ) where

import Prelude

import Data.Array.IArray
import Data.Array.Base

-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Ix
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Ix.hs,v 1.2 2001/12/21 15:07:21 simonmar Exp $
--
-- Class of index types.
--
-----------------------------------------------------------------------------

module Data.Ix
    (
	Ix
	  ( range	-- :: (Ix a) => (a,a) -> [a]
	  , index       -- :: (Ix a) => (a,a) -> a   -> Int
	  , inRange     -- :: (Ix a) => (a,a) -> a   -> Bool
	  , rangeSize	-- :: (Ix a) => (a,a) -> Int
	  )
    -- Ix instances:
    --
    --  Ix Char
    --  Ix Int
    --  Ix Integer
    --  Ix Bool
    --  Ix Ordering
    --  Ix ()
    --  (Ix a, Ix b) => Ix (a, b)
    --  ...

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.
    ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Arr
#endif

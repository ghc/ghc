-----------------------------------------------------------------------------
-- 
-- Module      :  Text.Show.Functions
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Functions.hs,v 1.1 2001/06/28 14:15:04 simonmar Exp $
--
-- Optional instance of Text.Show.Show for functions.
--
-----------------------------------------------------------------------------

module Text.Show.Functions where

import Prelude

instance Show (a -> b) where
	showsPrec _ _ = showString "<function>"

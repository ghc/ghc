-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Monad.ST.Strict
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- $Id: Strict.hs,v 1.2 2001/07/03 11:37:49 simonmar Exp $
--
-- The strict ST monad (identical to Control.Monad.ST)
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Strict (
 	module Control.Monad.ST
  ) where

import Prelude
import Control.Monad.ST

-----------------------------------------------------------------------------
-- 
-- Module      :  Text.PrettyPrint
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: PrettyPrint.hs,v 1.2 2001/09/13 11:39:14 simonmar Exp $
--
-- Re-export of Text.PrettyPrint.HughesPJ to provide a default
-- pretty-printing library.  Marked experimental at the moment so we
-- can change the default later if necessary.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint ( 
 	module Text.PrettyPrint.HughesPJ
 ) where

import Prelude
import Text.PrettyPrint.HughesPJ

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Data types for generic definitions.
--
-----------------------------------------------------------------------------

module Data.Generics ( 
	(:*:)(..), (:+:)(..), Unit(..)
 ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Base ( (:*:)(..), (:+:)(..), Unit(..) )
#endif

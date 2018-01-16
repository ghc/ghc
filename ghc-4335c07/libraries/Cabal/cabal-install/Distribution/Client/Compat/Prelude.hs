{-# LANGUAGE CPP #-}

-- to suppress WARNING in "Distribution.Compat.Prelude.Internal"
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | This module does two things:
--
-- * Acts as a compatiblity layer, like @base-compat@.
--
-- * Provides commonly used imports.
--
-- This module is a superset of "Distribution.Compat.Prelude" (which
-- this module re-exports)
--
module Distribution.Client.Compat.Prelude
  ( module Distribution.Compat.Prelude.Internal
  , Prelude.IO
  , readMaybe
  ) where

import Prelude (IO)
import Distribution.Compat.Prelude.Internal hiding (IO)

#if MIN_VERSION_base(4,6,0)
import Text.Read
         ( readMaybe )
#endif

#if !MIN_VERSION_base(4,6,0)
-- | An implementation of readMaybe, for compatibility with older base versions.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing
#endif

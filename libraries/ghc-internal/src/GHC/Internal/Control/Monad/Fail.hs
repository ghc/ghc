{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  GHC.Internal.Control.Monad.Fail
-- Copyright   :  (C) 2015 David Luposchainsky,
--                (C) 2015 Herbert Valerio Riedel
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
module GHC.Internal.Control.Monad.Fail ( MonadFail(fail) ) where

import GHC.Internal.Base (String, Monad(), Maybe(Nothing), IO(), (.))
import {-# SOURCE #-} GHC.Internal.IO (throwIO)
import {-# SOURCE #-} GHC.Internal.IO.Exception (userError)
import GHC.Internal.Stack.Types (HasCallStack)

-- | When a value is bound in @do@-notation, the pattern on the left
-- hand side of @<-@ might not match. In this case, this class
-- provides a function to recover.
--
-- A 'Monad' without a 'MonadFail' instance may only be used in conjunction
-- with pattern that always match, such as newtypes, tuples, data types with
-- only a single data constructor, and irrefutable patterns (@~pat@).
--
-- Instances of 'MonadFail' should satisfy the following law: @fail s@ should
-- be a left zero for 'Control.Monad.>>=',
--
-- @
-- fail s >>= f  =  fail s
-- @
--
-- If your 'Monad' is also 'Control.Monad.MonadPlus', a popular definition is
--
-- @
-- fail _ = mzero
-- @
--
-- @fail s@ should be an action that runs in the monad itself, not an
-- exception (except in instances of @MonadIO@).  In particular,
-- @fail@ should not be implemented in terms of @error@.
--
-- @since base-4.9.0.0
class Monad m => MonadFail m where
    fail :: HasCallStack => String -> m a


-- | @since base-4.9.0.0
instance MonadFail Maybe where
    fail :: HasCallStack => String -> Maybe a
    fail _ = Nothing

-- | @since base-4.9.0.0
instance MonadFail [] where
    {-# INLINE fail #-}
    fail :: HasCallStack => String -> [a]
    fail _ = []

-- | @since base-4.9.0.0
instance MonadFail IO where
    fail :: HasCallStack => String -> IO a
    fail = throwIO . userError

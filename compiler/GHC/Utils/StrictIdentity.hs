-- |
-- Module      :  GHC.Utils.StrictIdentity
-- License     :  BSD-style (see the file LICENSE)
--
-- The /strict/ identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.

-----------------------------------------------------------------------------

module GHC.Utils.StrictIdentity (
    StrictIdentity(..)
  ) where

import GHC.Prelude

newtype StrictIdentity a = StrictIdentity { runStrictIdentity :: a }

-----------------------------------------------------------------------------
-- In the following instances, the key lines are the strict applications ($!)
-----------------------------------------------------------------------------

instance Functor StrictIdentity where
    fmap f (StrictIdentity x) = StrictIdentity (f $! x)

instance Applicative StrictIdentity where
    pure = StrictIdentity
    (StrictIdentity f) <*> (StrictIdentity x) = StrictIdentity (f $! x)

instance Monad StrictIdentity where
    m >>= k  = k $! (runStrictIdentity m)

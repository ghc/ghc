{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Monad
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Functor', 'Monad' and 'MonadPlus' classes,
-- with some useful operations on monads.

module Control.Monad
    (-- *  Functor and monad classes
     Functor(..),
     Monad((>>=), (>>), return),
     MonadFail(fail),
     MonadPlus(mzero, mplus),
     -- *  Functions
     -- **  Naming conventions
     -- $naming
     -- **  Basic @Monad@ functions
     mapM,
     mapM_,
     forM,
     forM_,
     sequence,
     sequence_,
     (=<<),
     (>=>),
     (<=<),
     forever,
     void,
     -- **  Generalisations of list functions
     join,
     msum,
     mfilter,
     filterM,
     mapAndUnzipM,
     zipWithM,
     zipWithM_,
     foldM,
     foldM_,
     replicateM,
     replicateM_,
     -- **  Conditional execution of monadic expressions
     guard,
     when,
     unless,
     -- **  Monadic lifting operators
     liftM,
     liftM2,
     liftM3,
     liftM4,
     liftM5,
     ap,
     thenM,
     -- **  Strict monadic functions
     (<$!>)
     ) where

import GHC.Internal.Control.Monad
#if __GLASGOW_HASKELL__ < 1000
import Data.Function (const)
#endif

{- $naming

The functions in this module use the following naming conventions:

* A postfix \'@M@\' always stands for a function in the Kleisli category:
  The monad type constructor @m@ is added to function results
  (modulo currying) and nowhere else.  So, for example,

> filter  ::              (a ->   Bool) -> [a] ->   [a]
> filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

* A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
  Thus, for example:

> sequence  :: Monad m => [m a] -> m [a]
> sequence_ :: Monad m => [m a] -> m ()

* A prefix \'@m@\' generalizes an existing function to a monadic form.
  Thus, for example:

> filter  ::                (a -> Bool) -> [a] -> [a]
> mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a

-}

#if __GLASGOW_HASKELL__ < 1000

-- | Sequence two monadic actions, discarding the result of the first one.
--
-- Defined as `thenM ma mb = ma >>= const mb`.
--
-- This can be used to define `(*>) = thenM`.
--
-- @since 4.23.0.0
thenM :: (Monad m) => m a -> m b -> m b
thenM ma mb = ma >>= const mb
{-# INLINEABLE thenM #-}

#endif

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monadic fixpoints.
--
-- For a detailed discussion, see Levent Erkok's thesis,
-- /Value Recursion in Monadic Computations/, Oregon Graduate Institute, 2002.
--
-----------------------------------------------------------------------------

module Control.Monad.Fix (
        MonadFix(mfix),
        fix
  ) where

import Prelude
import System.IO
import Data.Function (fix)
#ifdef __HUGS__
import Hugs.Prelude (MonadFix(mfix))
#endif
#if defined(__GLASGOW_HASKELL__)
import GHC.ST
#endif

#ifndef __HUGS__
-- | Monads having fixed points with a \'knot-tying\' semantics.
-- Instances of 'MonadFix' should satisfy the following laws:
--
-- [/purity/]
--      @'mfix' ('return' . h)  =  'return' ('fix' h)@
--
-- [/left shrinking/ (or /tightening/)]
--      @'mfix' (\\x -> a >>= \\y -> f x y)  =  a >>= \\y -> 'mfix' (\\x -> f x y)@
--
-- [/sliding/]
--      @'mfix' ('Control.Monad.liftM' h . f)  =  'Control.Monad.liftM' h ('mfix' (f . h))@,
--      for strict @h@.
--
-- [/nesting/]
--      @'mfix' (\\x -> 'mfix' (\\y -> f x y))  =  'mfix' (\\x -> f x x)@
--
-- This class is used in the translation of the recursive @do@ notation
-- supported by GHC and Hugs.
class (Monad m) => MonadFix m where
        -- | The fixed point of a monadic computation.
        -- @'mfix' f@ executes the action @f@ only once, with the eventual
        -- output fed back as the input.  Hence @f@ should not be strict,
        -- for then @'mfix' f@ would diverge.
        mfix :: (a -> m a) -> m a
#endif /* !__HUGS__ */

-- Instances of MonadFix for Prelude monads

instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x
                   unJust Nothing  = error "mfix Maybe: Nothing"

instance MonadFix [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)

instance MonadFix IO where
    mfix = fixIO 

instance MonadFix ((->) r) where
    mfix f = \ r -> let a = f a r in a

instance MonadFix (Either e) where
    mfix f = let a = f (unRight a) in a
             where unRight (Right x) = x
                   unRight (Left  _) = error "mfix Either: Left"

#if defined(__GLASGOW_HASKELL__)
instance MonadFix (ST s) where
        mfix = fixST
#endif


{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
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

import Data.Either
import Data.Function ( fix )
import Data.Maybe
import Data.Monoid ( Dual(..), Sum(..), Product(..)
                   , First(..), Last(..), Alt(..), Ap(..) )
import Data.Ord ( Down(..) )
import GHC.Base ( Monad, NonEmpty(..), errorWithoutStackTrace, (.) )
import GHC.Generics
import GHC.List ( head, tail )
import GHC.Tuple (Solo (..))
import Control.Monad.ST.Imp
import System.IO

-- | Monads having fixed points with a \'knot-tying\' semantics.
-- Instances of 'MonadFix' should satisfy the following laws:
--
-- [Purity]
--      @'mfix' ('Control.Monad.return' . h)  =  'Control.Monad.return' ('fix' h)@
--
-- [Left shrinking (or Tightening)]
--      @'mfix' (\\x -> a >>= \\y -> f x y)  =  a >>= \\y -> 'mfix' (\\x -> f x y)@
--
-- [Sliding]
--      @'mfix' ('Control.Monad.liftM' h . f)  =  'Control.Monad.liftM' h ('mfix' (f . h))@,
--      for strict @h@.
--
-- [Nesting]
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

-- Instances of MonadFix for Prelude monads

-- | @since 4.15
instance MonadFix Solo where
    mfix f = let a = f (unSolo a) in a
             where unSolo (Solo x) = x

-- | @since 2.01
instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x
                   unJust Nothing  = errorWithoutStackTrace "mfix Maybe: Nothing"

-- | @since 2.01
instance MonadFix [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)

-- | @since 4.9.0.0
instance MonadFix NonEmpty where
  mfix f = case fix (f . neHead) of
             ~(x :| _) -> x :| mfix (neTail . f)
    where
      neHead ~(a :| _) = a
      neTail ~(_ :| as) = as

-- | @since 2.01
instance MonadFix IO where
    mfix = fixIO

-- | @since 2.01
instance MonadFix ((->) r) where
    mfix f = \ r -> let a = f a r in a

-- | @since 4.3.0.0
instance MonadFix (Either e) where
    mfix f = let a = f (unRight a) in a
             where unRight (Right x) = x
                   unRight (Left  _) = errorWithoutStackTrace "mfix Either: Left"

-- | @since 2.01
instance MonadFix (ST s) where
        mfix = fixST

-- Instances of Data.Monoid wrappers

-- | @since 4.8.0.0
instance MonadFix Dual where
    mfix f   = Dual (fix (getDual . f))

-- | @since 4.8.0.0
instance MonadFix Sum where
    mfix f   = Sum (fix (getSum . f))

-- | @since 4.8.0.0
instance MonadFix Product where
    mfix f   = Product (fix (getProduct . f))

-- | @since 4.8.0.0
instance MonadFix First where
    mfix f   = First (mfix (getFirst . f))

-- | @since 4.8.0.0
instance MonadFix Last where
    mfix f   = Last (mfix (getLast . f))

-- | @since 4.8.0.0
instance MonadFix f => MonadFix (Alt f) where
    mfix f   = Alt (mfix (getAlt . f))

-- | @since 4.12.0.0
instance MonadFix f => MonadFix (Ap f) where
    mfix f   = Ap (mfix (getAp . f))

-- Instances for GHC.Generics
-- | @since 4.9.0.0
instance MonadFix Par1 where
    mfix f = Par1 (fix (unPar1 . f))

-- | @since 4.9.0.0
instance MonadFix f => MonadFix (Rec1 f) where
    mfix f = Rec1 (mfix (unRec1 . f))

-- | @since 4.9.0.0
instance MonadFix f => MonadFix (M1 i c f) where
    mfix f = M1 (mfix (unM1. f))

-- | @since 4.9.0.0
instance (MonadFix f, MonadFix g) => MonadFix (f :*: g) where
    mfix f = (mfix (fstP . f)) :*: (mfix (sndP . f))
      where
        fstP (a :*: _) = a
        sndP (_ :*: b) = b

-- Instances for Data.Ord

-- | @since 4.12.0.0
instance MonadFix Down where
    mfix f = Down (fix (getDown . f))

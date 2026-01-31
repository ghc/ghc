{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

-- For head in instance MonadFix []
{-# OPTIONS_GHC -Wno-x-partial #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Control.Monad.Fix
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

module GHC.Internal.Control.Monad.Fix (
        MonadFix(mfix),
        fix,
        fixIO
  ) where

import GHC.Internal.Data.Either
import GHC.Internal.Data.Function ( fix )
import GHC.Internal.Data.Maybe
import GHC.Internal.Data.Monoid ( Monoid, Dual(..), Sum(..), Product(..)
                   , First(..), Last(..), Alt(..), Ap(..) )
import GHC.Internal.Data.NonEmpty ( NonEmpty(..) )
import GHC.Internal.Data.Tuple ( Solo(..), snd )
import GHC.Internal.Base ( Monad, errorWithoutStackTrace, (.), return )
import GHC.Internal.List ( head, drop )
import GHC.Internal.Control.Monad.ST
import GHC.Internal.Data.Functor.Identity (Identity(..))
import GHC.Internal.Generics
import GHC.Internal.IO
import GHC.Internal.IO.Exception
import GHC.Internal.IO.Unsafe ()
import GHC.Internal.MVar

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

-- | @since base-4.15
instance MonadFix Solo where
    mfix f = let a = f (unSolo a) in a
             where unSolo (MkSolo x) = x

-- | @since base-4.21
instance Monoid a => MonadFix ((,) a) where
    -- See the CLC proposal thread for discussion and proofs of the laws: https://github.com/haskell/core-libraries-committee/issues/238
    mfix f = let a = f (snd a) in a

-- | @since base-2.01
instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x
                   unJust Nothing  = errorWithoutStackTrace "mfix Maybe: Nothing"

-- | @since base-2.01
instance MonadFix [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (drop 1 . f)

-- | @since base-4.9.0.0
instance MonadFix NonEmpty where
  mfix f = case fix (f . neHead) of
             ~(x :| _) -> x :| mfix (neTail . f)
    where
      neHead ~(a :| _) = a
      neTail ~(_ :| as) = as

-- ---------------------------------------------------------------------------
-- fixIO

-- | The implementation of 'Control.Monad.Fix.mfix' for 'IO'.
--
-- This operation may fail with:
--
-- * 'FixIOException' if the function passed to 'fixIO' inspects its argument.
--
-- ==== __Examples__
--
-- the IO-action is only executed once. The recursion is only on the values.
--
-- >>> take 3 <$> fixIO (\x -> putStr ":D" >> (:x) <$> readLn @Int)
-- :D
-- 2
-- [2,2,2]
--
-- If we are strict in the value, just as with 'Data.Function.fix', we do not get termination:
--
-- >>> fixIO (\x -> putStr x >> pure ('x' : x))
-- * hangs forever *
--
-- We can tie the knot of a structure within 'IO' using 'fixIO':
--
-- @
-- data Node = MkNode Int (IORef Node)
--
-- foo :: IO ()
-- foo = do
--   p \<- fixIO (\p -> newIORef (MkNode 0 p))
--   q <- output p
--   r <- output q
--   _ <- output r
--   pure ()
--
-- output :: IORef Node -> IO (IORef Node)
-- output ref = do
--   MkNode x p <- readIORef ref
--   print x
--   pure p
-- @
--
-- >>> foo
-- 0
-- 0
-- 0
fixIO :: (a -> IO a) -> IO a
fixIO k = do
    m <- newEmptyMVar
    ans <- unsafeDupableInterleaveIO
             (readMVar m `catch` \BlockedIndefinitelyOnMVar ->
                                    throwIO FixIOException)
    result <- k ans
    putMVar m result
    return result

-- Note [Blackholing in fixIO]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We do our own explicit black holing here, because GHC's lazy
-- blackholing isn't enough.  In an infinite loop, GHC may run the IO
-- computation a few times before it notices the loop, which is wrong.
--
-- NOTE2: the explicit black-holing with an IORef ran into trouble
-- with multiple threads (see #5421), so now we use an MVar. We used
-- to use takeMVar with unsafeInterleaveIO. This, however, uses noDuplicate#,
-- which is not particularly cheap. Better to use readMVar, which can be
-- performed in multiple threads safely, and to use unsafeDupableInterleaveIO
-- to avoid the noDuplicate cost.
--
-- What we'd ideally want is probably an IVar, but we don't quite have those.
-- STM TVars look like an option at first, but I don't think they are:
-- we'd need to be able to write to the variable in an IO context, which can
-- only be done using 'atomically', and 'atomically' is not allowed within
-- unsafePerformIO. We can't know if someone will try to use the result
-- of fixIO with unsafePerformIO!
--
-- See also System.IO.Unsafe.unsafeFixIO.
--

-- | @since base-2.01
instance MonadFix IO where
    mfix = fixIO

-- | @since base-2.01
instance MonadFix ((->) r) where
    mfix f = \ r -> let a = f a r in a

-- | @since base-4.3.0.0
instance MonadFix (Either e) where
    mfix f = let a = f (unRight a) in a
             where unRight (Right x) = x
                   unRight (Left  _) = errorWithoutStackTrace "mfix Either: Left"

-- | @since base-2.01
instance MonadFix (ST s) where
        mfix = fixST

-- Instances of Data.Monoid wrappers

-- | @since base-4.8.0.0
instance MonadFix Dual where
    mfix f   = Dual (fix (getDual . f))

-- | @since base-4.8.0.0
instance MonadFix Sum where
    mfix f   = Sum (fix (getSum . f))

-- | @since base-4.8.0.0
instance MonadFix Product where
    mfix f   = Product (fix (getProduct . f))

-- | @since base-4.8.0.0
instance MonadFix First where
    mfix f   = First (mfix (getFirst . f))

-- | @since base-4.8.0.0
instance MonadFix Last where
    mfix f   = Last (mfix (getLast . f))

-- | @since base-4.8.0.0
instance MonadFix f => MonadFix (Alt f) where
    mfix f   = Alt (mfix (getAlt . f))

-- | @since base-4.12.0.0
instance MonadFix f => MonadFix (Ap f) where
    mfix f   = Ap (mfix (getAp . f))

-- | @since base-4.8.0.0
instance MonadFix Identity where
    mfix f   = Identity (fix (runIdentity . f))

-- | @since base-4.9.0.0
instance MonadFix Par1 where
    mfix f = Par1 (fix (unPar1 . f))

-- | @since base-4.9.0.0
instance MonadFix f => MonadFix (Rec1 f) where
    mfix f = Rec1 (mfix (unRec1 . f))

-- | @since base-4.9.0.0
instance MonadFix f => MonadFix (M1 i c f) where
    mfix f = M1 (mfix (unM1. f))

-- | @since base-4.9.0.0
instance (MonadFix f, MonadFix g) => MonadFix (f :*: g) where
    mfix f = (mfix (fstP . f)) :*: (mfix (sndP . f))
      where
        fstP (a :*: _) = a
        sndP (_ :*: b) = b

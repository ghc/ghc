{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IORef
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The IORef type
--
-----------------------------------------------------------------------------

module GHC.IORef (
        IORef(..),
        newIORef, readIORef, writeIORef, atomicModifyIORef2Lazy,
        atomicModifyIORef2, atomicModifyIORefLazy_, atomicModifyIORef'_,
        atomicModifyIORefP, atomicSwapIORef, atomicModifyIORef'
    ) where

import GHC.Base
import GHC.STRef
import GHC.IO

-- ---------------------------------------------------------------------------
-- IORefs

-- |A mutable variable in the 'IO' monad
newtype IORef a = IORef (STRef RealWorld a)
  deriving Eq
  -- ^ Pointer equality.
  --
  -- @since 4.0.0.0

-- |Build a new 'IORef'
newIORef    :: a -> IO (IORef a)
newIORef v = stToIO (newSTRef v) >>= \ var -> return (IORef var)

-- |Read the value of an 'IORef'
readIORef   :: IORef a -> IO a
readIORef  (IORef var) = stToIO (readSTRef var)

-- |Write a new value into an 'IORef'
writeIORef  :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeSTRef var v)

-- Atomically apply a function to the contents of an 'IORef',
-- installing its first component in the 'IORef' and returning
-- the old contents and the result of applying the function.
-- The result of the function application (the pair) is not forced.
-- As a result, this can lead to memory leaks. It is generally better
-- to use 'atomicModifyIORef2'.
atomicModifyIORef2Lazy :: IORef a -> (a -> (a,b)) -> IO (a, (a, b))
atomicModifyIORef2Lazy (IORef (STRef r#)) f =
  IO (\s -> case atomicModifyMutVar2# r# f s of
              (# s', old, res #) -> (# s', (old, res) #))

-- Atomically apply a function to the contents of an 'IORef',
-- installing its first component in the 'IORef' and returning
-- the old contents and the result of applying the function.
-- The result of the function application (the pair) is forced,
-- but neither of its components is.
atomicModifyIORef2 :: IORef a -> (a -> (a,b)) -> IO (a, (a, b))
atomicModifyIORef2 ref f = do
  r@(_old, (_new, _res)) <- atomicModifyIORef2Lazy ref f
  return r

-- | A version of 'Data.IORef.atomicModifyIORef' that forces
-- the (pair) result of the function.
atomicModifyIORefP :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefP ref f = do
  (_old, (_,r)) <- atomicModifyIORef2 ref f
  pure r

-- | Atomically apply a function to the contents of an
-- 'IORef' and return the old and new values. The result
-- of the function is not forced. As this can lead to a
-- memory leak, it is usually better to use `atomicModifyIORef'_`.
atomicModifyIORefLazy_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORefLazy_ (IORef (STRef ref)) f = IO $ \s ->
  case atomicModifyMutVar_# ref f s of
    (# s', old, new #) -> (# s', (old, new) #)

-- | Atomically apply a function to the contents of an
-- 'IORef' and return the old and new values. The result
-- of the function is forced.
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORef'_ ref f = do
  (old, !new) <- atomicModifyIORefLazy_ ref f
  return (old, new)

-- | Atomically replace the contents of an 'IORef', returning
-- the old contents.
atomicSwapIORef :: IORef a -> a -> IO a
-- Bad implementation! This will be a primop shortly.
atomicSwapIORef (IORef (STRef ref)) new = IO $ \s ->
  case atomicModifyMutVar2# ref (\_old -> Box new) s of
    (# s', old, Box _new #) -> (# s', old #)

data Box a = Box a

-- | Strict version of 'Data.IORef.atomicModifyIORef'. This forces both
-- the value stored in the 'IORef' and the value returned. The new value
-- is installed in the 'IORef' before the returned value is forced.
-- So
--
-- @atomicModifyIORef' ref (\x -> (x+1, undefined))@
--
-- will increment the 'IORef' and then throw an exception in the calling
-- thread.
--
-- @since 4.6.0.0
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
-- See Note [atomicModifyIORef' definition]
atomicModifyIORef' ref f = do
  (_old, (_new, !res)) <- atomicModifyIORef2 ref $
    \old -> case f old of
       r@(!_new, _res) -> r
  pure res

-- Note [atomicModifyIORef' definition]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- atomicModifyIORef' was historically defined
--
--    atomicModifyIORef' ref f = do
--        b <- atomicModifyIORef ref $ \a ->
--                case f a of
--                    v@(a',_) -> a' `seq` v
--        b `seq` return b
--
-- The most obvious definition, now that we have atomicModifyMutVar2#,
-- would be
--
--    atomicModifyIORef' ref f = do
--      (_old, (!_new, !res)) <- atomicModifyIORef2 ref f
--      pure res
--
-- Why do we force the new value on the "inside" instead of afterwards?
-- I initially thought the latter would be okay, but then I realized
-- that if we write
--
--   atomicModifyIORef' ref $ \x -> (x + 5, x - 5)
--
-- then we'll end up building a pair of thunks to calculate x + 5
-- and x - 5. That's no good! With the more complicated definition,
-- we avoid this problem; the result pair is strict in the new IORef
-- contents. Of course, if the function passed to atomicModifyIORef'
-- doesn't inline, we'll build a closure for it. But that was already
-- true for the historical definition of atomicModifyIORef' (in terms
-- of atomicModifyIORef), so we shouldn't lose anything. Note that
-- in keeping with the historical behavior, we *don't* propagate the
-- strict demand on the result inwards. In particular,
--
--   atomicModifyIORef' ref (\x -> (x + 1, undefined))
--
-- will increment the IORef and throw an exception; it will not
-- install an undefined value in the IORef.
--
-- A clearer version, in my opinion (but one quite incompatible with
-- the traditional one) would only force the new IORef value and not
-- the result. This version would have been relatively inefficient
-- to implement using atomicModifyMutVar#, but is just fine now.

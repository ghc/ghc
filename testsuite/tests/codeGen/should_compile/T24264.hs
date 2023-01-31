{-# LANGUAGE MagicHash #-}

module T24264 where

import Control.Exception (evaluate)
import GHC.Exts (seq#, noinline)
import GHC.IO (IO(..))

fun1 :: a -> IO a
{-# OPAQUE fun1 #-}
fun1 x = do
  pure ()
  pure $! x
  -- This should not push a continuation to the stack before entering 'x'

fun2 :: a -> IO a
{-# OPAQUE fun2 #-}
fun2 x = do
  pure ()
  evaluate x
  -- This should not push a continuation to the stack before entering 'x'

fun3 :: a -> IO a
{-# OPAQUE fun3 #-}
fun3 x = do
  pure ()
  evaluate $! x
  -- This ideally also should not push a continuation to the stack
  -- before entering 'x'.

funPair :: a -> IO (a, a)
{-# OPAQUE funPair #-}
funPair x = do
  pure ()
  x' <- pure $! x
  -- This should push a continuation to the stack before entering 'x',
  -- so the pair can be returned instead.  (It's here to make sure
  -- that the 'returns to' detection continues working correctly.)
  pure (x', x')

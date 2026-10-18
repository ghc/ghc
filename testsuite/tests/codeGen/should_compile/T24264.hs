{-# LANGUAGE MagicHash #-}

module T24264 where

import Control.Exception (evaluate)
import GHC.Exts (seq#, noinline)
import GHC.IO (IO(..))

-- This should not push a continuation to the stack before entering 'x':
-- CHECK-LABEL: T24264.fun1_entry()
-- CHECK:       call stg_ap_0_fast(R1) args:
-- CHECK-NOT:   returns to
fun1 :: a -> IO a
{-# OPAQUE fun1 #-}
fun1 x = do
  pure ()
  pure $! x

-- This should not push a continuation to the stack before entering 'x':
-- CHECK-LABEL: T24264.fun2_entry()
-- CHECK:       call stg_ap_0_fast(R1) args:
-- CHECK-NOT:   returns to
fun2 :: a -> IO a
{-# OPAQUE fun2 #-}
fun2 x = do
  pure ()
  evaluate x

-- This ideally also should not push a continuation to the stack
-- before entering 'x', but currently does:
-- CHECK-LABEL: T24264.fun3_entry()
-- CHECK:       call stg_ap_0_fast(R1) returns to
fun3 :: a -> IO a
{-# OPAQUE fun3 #-}
fun3 x = do
  pure ()
  evaluate $! x

-- This should push a continuation to the stack before entering 'x',
-- so the pair can be returned instead.  (It's here to make sure
-- that the 'returns to' detection continues working correctly.)
-- CHECK-LABEL: T24264.funPair_entry()
-- CHECK:       call stg_ap_0_fast(R1) returns to
funPair :: a -> IO (a, a)
{-# OPAQUE funPair #-}
funPair x = do
  pure ()
  x' <- pure $! x
  pure (x', x')

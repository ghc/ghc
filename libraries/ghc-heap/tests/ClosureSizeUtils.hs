{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities for the @closure_size@ tests
module ClosureSizeUtils (assertSize, assertSizeUnlifted) where

import Control.Monad
import GHC.Exts
import GHC.Exts.Heap.Closures
import GHC.Stack
import Type.Reflection

profHeaderSize :: Int
#if PROFILING
profHeaderSize = 2
#else
profHeaderSize = 0
#endif

assertSize
  :: forall a. (HasCallStack, Typeable a)
  => a     -- ^ closure
  -> Int   -- ^ expected size in words
  -> IO ()
assertSize x =
  assertSizeBox (asBox x) (typeRep @a)

assertSizeUnlifted
  :: forall (a :: TYPE 'UnliftedRep). (HasCallStack, Typeable a)
  => a     -- ^ closure
  -> Int   -- ^ expected size in words
  -> IO ()
assertSizeUnlifted x =
  assertSizeBox (Box (unsafeCoerce# x)) (typeRep @a)

assertSizeBox
  :: forall a. (HasCallStack)
  => Box   -- ^ closure
  -> TypeRep a
  -> Int   -- ^ expected size in words
  -> IO ()
assertSizeBox x ty expected = do
  let !size = closureSize x
  when (size /= expected') $ do
    putStrLn $ "closureSize ("++show ty++") == "++show size++", expected "++show expected'
    putStrLn $ prettyCallStack callStack
  where expected' = expected + profHeaderSize
{-# NOINLINE assertSize #-}

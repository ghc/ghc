{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

import Control.Monad
import Type.Reflection
import GHC.Exts
import GHC.Stack
import GHC.IO

import GHC.Exts.Heap.Closures

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

pap :: Int -> Char -> Int
pap x _ = x
{-# NOINLINE pap #-}

profHeaderSize :: Int
#if PROFILING
profHeaderSize = 2
#else
profHeaderSize = 0
#endif

data A = A (Array# Int)
data MA = MA (MutableArray# RealWorld Int)
data BA = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)
data B = B BCO#
data APC a = APC a


main :: IO ()
main = do
  assertSize 'a' 2
  assertSize (Just ()) 2
  assertSize (Nothing :: Maybe ()) 2
  assertSize ((1,2) :: (Int,Int)) 3
  assertSize ((1,2,3) :: (Int,Int,Int)) 4

  -- These depend too much upon the behavior of the simplifier to
  -- test reliably.
  --assertSize (id :: Int -> Int) 1
  --assertSize (fst :: (Int,Int) -> Int) 1
  --assertSize (pap 1) 2

  MA ma <- IO $ \s ->
      case newArray# 0# 0 s of
          (# s1, x #) -> (# s1, MA x #)

  A a <- IO $ \s ->
      case freezeArray# ma 0# 0# s of
          (# s1, x #) -> (# s1, A x #)

  MBA mba <- IO $ \s ->
      case newByteArray# 0# s of
          (# s1, x #) -> (# s1, MBA x #)

  BA ba <- IO $ \s ->
      case newByteArray# 0# s of
          (# s1, x #) ->
              case unsafeFreezeByteArray# x s1 of
                  (# s2, y #) -> (# s2, BA y #)

  assertSizeUnlifted ma 3
  assertSizeUnlifted a 3
  assertSizeUnlifted mba 2
  assertSizeUnlifted ba 2

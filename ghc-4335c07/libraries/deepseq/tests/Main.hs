-- Code reused from http://hackage.haskell.org/package/deepseq-generics

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Typeable
import Data.Word
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

-- import Test.Framework (defaultMain, testGroup, testCase)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

-- IUT
import Control.DeepSeq

-- needed for GHC-7.4 compatibility
#if !MIN_VERSION_base(4,6,0)
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
#endif

----------------------------------------------------------------------------
-- simple hacky abstraction for testing forced evaluation via `rnf`-like functions

seqStateLock :: MVar ()
seqStateLock = unsafePerformIO $ newMVar ()
{-# NOINLINE seqStateLock #-}

withSeqState :: Word64 -> IO () -> IO ()
withSeqState expectedState act = withMVar seqStateLock $ \() -> do
    0  <- resetSeqState
    () <- act
    st <- resetSeqState
    unless (st == expectedState) $
        assertFailure ("withSeqState: actual seq-state ("++show st++") doesn't match expected value ("++
                       show expectedState++")")

seqState :: IORef Word64
seqState = unsafePerformIO $ newIORef 0
{-# NOINLINE seqState #-}

resetSeqState :: IO Word64
resetSeqState = atomicModifyIORef' seqState (0,)

-- |Set flag and raise exception is flag already set
setSeqState :: Int -> IO ()
setSeqState i | 0 <= i && i < 64 = atomicModifyIORef' seqState go
              | otherwise        = error "seqSeqState: flag index must be in [0..63]"
  where
    go x | testBit x i = error ("setSeqState: flag #"++show i++" already set")
         | otherwise   = (setBit x i, ())

-- weird type whose NFData instacne calls 'setSeqState' when rnf-ed
data SeqSet = SeqSet !Int | SeqIgnore
              deriving Show

instance NFData SeqSet where
    rnf (SeqSet i)  = unsafePerformIO $ setSeqState i
    rnf (SeqIgnore) = ()
    {-# NOINLINE rnf #-}

-- |Exception to be thrown for testing 'seq'/'rnf'
data RnfEx = RnfEx deriving (Eq, Show, Typeable)

instance Exception RnfEx

instance NFData RnfEx where rnf e = throw e

assertRnfEx :: () -> IO ()
assertRnfEx v = handleJust isWanted (const $ return ()) $ do
    () <- evaluate v
    assertFailure "failed to trigger expected RnfEx exception"
  where isWanted = guard . (== RnfEx)

----------------------------------------------------------------------------

case_1, case_2, case_3 :: Test.Framework.Test
case_4_1, case_4_2, case_4_3, case_4_4 :: Test.Framework.Test
#if __GLASGOW_HASKELL__ >= 706
case_4_1b, case_4_2b, case_4_3b, case_4_4b :: Test.Framework.Test
#endif

newtype Case1 = Case1 Int
              deriving (Generic)

instance NFData Case1

case_1 = testCase "Case1" $ do
    assertRnfEx $ rnf $ (Case1 (throw RnfEx))

----

data Case2 = Case2 Int
           deriving (Generic)

instance NFData Case2

case_2 = testCase "Case2" $ do
    assertRnfEx $ rnf $ (Case2 (throw RnfEx))

----

data Case3 = Case3 RnfEx
           deriving (Generic)

instance NFData Case3

case_3 = testCase "Case3" $ do
    assertRnfEx $ rnf $ Case3 RnfEx

----

data Case4 a = Case4a
             | Case4b a a
             | Case4c a (Case4 a)
             deriving ( Generic
#if __GLASGOW_HASKELL__ >= 706
                      , Generic1
#endif
                      )

instance NFData a => NFData (Case4 a)

#if __GLASGOW_HASKELL__ >= 706
instance NFData1 Case4
#endif

case_4_1 = testCase "Case4.1" $ withSeqState 0x0 $ do
    evaluate $ rnf $ (Case4a :: Case4 SeqSet)

case_4_2 = testCase "Case4.2" $ withSeqState 0x3 $ do
    evaluate $ rnf $ (Case4b (SeqSet 0) (SeqSet 1) :: Case4 SeqSet)

case_4_3 = testCase "Case4.3" $ withSeqState (bit 55) $ do
    evaluate $ rnf $ (Case4b SeqIgnore (SeqSet 55) :: Case4 SeqSet)

case_4_4 = testCase "Case4.4" $ withSeqState 0xffffffffffffffff $ do
    evaluate $ rnf $ (genCase 63)
  where
    genCase n | n > 1      = Case4c (SeqSet n) (genCase (n-1))
              | otherwise  = Case4b (SeqSet 0) (SeqSet 1)

#if __GLASGOW_HASKELL__ >= 706
case_4_1b = testCase "Case4.1b" $ withSeqState 0x0 $ do
    evaluate $ rnf1 $ (Case4a :: Case4 SeqSet)

case_4_2b = testCase "Case4.2b" $ withSeqState 0x3 $ do
    evaluate $ rnf1 $ (Case4b (SeqSet 0) (SeqSet 1) :: Case4 SeqSet)

case_4_3b = testCase "Case4.3b" $ withSeqState (bit 55) $ do
    evaluate $ rnf1 $ (Case4b SeqIgnore (SeqSet 55) :: Case4 SeqSet)

case_4_4b = testCase "Case4.4b" $ withSeqState 0xffffffffffffffff $ do
    evaluate $ rnf1 $ (genCase 63)
  where
    genCase n | n > 1      = Case4c (SeqSet n) (genCase (n-1))
              | otherwise  = Case4b (SeqSet 0) (SeqSet 1)
#endif

----------------------------------------------------------------------------

main :: IO ()
main = defaultMain [tests]
  where
    tests = testGroup ""
        [ case_1, case_2, case_3
        , case_4_1, case_4_2, case_4_3, case_4_4
#if __GLASGOW_HASKELL__ >= 706
        , case_4_1b, case_4_2b, case_4_3b, case_4_4b
#endif
        ]

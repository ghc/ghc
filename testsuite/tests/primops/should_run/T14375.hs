-- Check that the bug from #14346 doesn't regress.
--
-- We currently have (at least) two remedies in place: the workaround of
-- marking @allocaBytes(Aligned)@ as @INLINE@, and the new @with#@ primop
-- described in #14375, which should solve the root cause.
--
-- To reproduce the problem, we need to trick the optimizer into considering
-- the end of the allocaBytes scope unreachable; we do this by using @forever@,
-- and then throwing an exception inside it after we have run enough iterations
-- to either trigger the bug or conclude that things are fine.

{-#LANGUAGE LambdaCase #-}

import System.Mem
import System.Mem.Weak
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Maybe
import Data.Word
import GHC.Prim
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Exception
import Text.Printf
import Numeric

newtype Stop = Stop String
  deriving (Show)

instance Exception Stop where

main = go `catch` handle
  where
    handle :: Stop -> IO ()
    handle (Stop e) = putStrLn e

go :: IO ()
go = do
  replicateM_ 1000 $ threadDelay 1
  allocaBytes 4 $ \p -> do
    performMajorGC
    poke p (0xdeadbeef :: Word32)
    forever $ do
      replicateM_ 10000 $ do
        threadDelay 10
        performMajorGC
        x <- peek p
        unless (x == 0xdeadbeef) $ do
          putStrLn $ showHex x ""
          throw (Stop "invalid") -- detected bug: abort.
      throw (Stop "OK") -- probably no bug: abort.

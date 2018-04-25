-- | Miscellaneous testing utilities
--
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Utils
    (
      (=^=)
    , withRedirect
    , withTempFile
    ) where

import Control.Exception (SomeException, bracket, bracket_, evaluate, try)
import Control.Monad (when)
import Debug.Trace (trace)
import GHC.IO.Handle.Internals (withHandle)
import System.Directory (removeFile)
import System.IO (Handle, hClose, hFlush, hIsOpen, hIsWritable, openTempFile)
import System.IO.Unsafe (unsafePerformIO)

-- Ensure that two potentially bottom values (in the sense of crashing
-- for some inputs, not looping infinitely) either both crash, or both
-- give comparable results for some input.
(=^=) :: (Eq a, Show a) => a -> a -> Bool
i =^= j = unsafePerformIO $ do
  x <- try (evaluate i)
  y <- try (evaluate j)
  case (x,y) of
    (Left (_ :: SomeException), Left (_ :: SomeException))
                       -> return True
    (Right a, Right b) -> return (a == b)
    e                  -> trace ("*** Divergence: " ++ show e) return False
infix 4 =^=
{-# NOINLINE (=^=) #-}

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = bracket (openTempFile "." "crashy.txt") cleanupTemp . uncurry
  where
    cleanupTemp (path,h) = do
      open <- hIsOpen h
      when open (hClose h)
      removeFile path

withRedirect :: Handle -> Handle -> IO a -> IO a
withRedirect tmp h = bracket_ swap swap
  where
    whenM p a = p >>= (`when` a)
    swap = do
      whenM (hIsOpen tmp) $ whenM (hIsWritable tmp) $ hFlush tmp
      whenM (hIsOpen h) $ whenM (hIsWritable h) $ hFlush h
      withHandle "spam" tmp $ \tmph -> do
        hh <- withHandle "spam" h $ \hh ->
          return (tmph,hh)
        return (hh,())

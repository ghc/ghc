-- Ben Gamari at #25300
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

-- We want to demonstrate (with and without 'unsafePerformIO') that 'bracket',
-- due to 'catch'ing and subsequently re-'throw'ing an exception,
-- can turn an asynchronous exception into a synchronous exception, resulting
-- in an otherwise terminating thunk becoming divergent.

module Main where

import GHC.Exts (catch#, raiseIO#)
import GHC.IO (IO(..), unIO)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc
import Control.Monad (void)
import Control.Exception (bracket, Exception, evaluate, fromException)

x :: Int
x = unsafePerformIO $ idBracket $ do
  putStrLn "entered x"
  threadDelay 1000000000
  putStrLn "leaving x"
  return 42

main :: IO ()
main = do
  t <- forkIO $ evaluate x >> return ()
  threadDelay 1000

  putStrLn "killing"
  killThread t

  putStrLn "printing"
  print x

idBracket :: IO a -> IO a
idBracket =
  id
#if defined(BRACKET)
  . bracket (return ()) return . const   -- Option B
#endif


{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Exit

test0 :: Maybe ()
test0 = do
  () <- Just undefined
  () <- Just undefined
  return ()

test1 :: Maybe ()
test1 = do
  (_,_) <- Just undefined
  return ()

test2 :: Maybe (Int,Int)
test2 = do
  x <- return 1
  () <- Just undefined
  y <- return 2
  return (x,y)

main = do
  b <- (print (isJust test0) >> return True)
   `catch` \ErrorCall{} -> return False
  when b $ die "failed0"
  b <- (print (isJust test1) >> return True)
   `catch` \ErrorCall{} -> return False
  when b $ die "failed1"
  b <- (print (isJust test2) >> return True)
   `catch` \ErrorCall{} -> return False
  when b $ die "failed2"

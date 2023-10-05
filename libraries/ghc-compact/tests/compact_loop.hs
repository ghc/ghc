module Main where

import Control.Exception
import System.Mem
import Text.Show

import GHC.Compact

assertFail :: String -> IO ()
assertFail msg = throwIO $ AssertionFailed msg

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals expected actual =
  if expected == actual then return ()
  else assertFail $ "expected " ++ (show expected)
       ++ ", got " ++ (show actual)

data Tree = Nil | Node Tree Tree Tree

instance Eq Tree where
  Nil == Nil = True
  Node _ l1 r1 == Node _ l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

instance Show Tree where
  showsPrec _ Nil = showString "Nil"
  showsPrec _ (Node _ l r) = showString "(Node " . shows l .
                             showString " " . shows r . showString ")"

{-# NOINLINE test #-}
test x = do
  let a = Node Nil x b
      b = Node a Nil Nil
  str <- compactSized 4096 True a

  -- check the value in the compact
  assertEquals a (getCompact str)
  performMajorGC
  -- check again the value in the compact
  assertEquals a (getCompact str)

main = test Nil

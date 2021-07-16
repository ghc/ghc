{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad (when, return)
import Data.Bool
import Data.Function (($))
import Debug.Trace (traceShow)
import GHC.Stack
import GHC.Types (Symbol)
import System.IO (IO, print)
import qualified Control.Monad

fromString :: HasCallStack => a -> CallStack
fromString _ = callStack

fromInteger :: HasCallStack => a -> CallStack
fromInteger _ = callStack

fromRational :: HasCallStack => a -> CallStack
fromRational _ = callStack

fromListN :: HasCallStack => len -> a -> CallStack
fromListN _len _ = callStack

fromLabel :: forall (_lbl::Symbol). HasCallStack => CallStack
fromLabel = callStack

ifThenElse :: HasCallStack => Bool -> a -> a -> CallStack
ifThenElse cond _ok _ko | cond = callStack
                        | otherwise = callStack

(>>) :: HasCallStack => a -> b -> CallStack
(>>) _a _b = callStack

negate :: HasCallStack => a -> CallStack
negate _a = callStack

(==) :: HasCallStack => a -> b -> Bool
(==) _a _b = traceShow callStack True

main :: IO ()
main = Control.Monad.do

  -- These come out on stdout
  print $ fromString "str"
  print $ "str"
  print $ fromLabel @"lbl"
  print $ #lbl
  print $ fromInteger 42
  print $ 42
  print $ fromRational 4.2
  print $ 4.2
  print $ fromListN () []
  print $ []
  print $ ifThenElse True () ()
  print $ if True then () else ()
  print $ negate 42
  print $ -42
  print $ () >> ()
  print $ do { (); () }

  -- These two come out in stderr, from traceShow
  when (42 == 42) $ return ()
  case 42 of
    42 -> return ()
  return ()

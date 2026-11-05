-- regression test for viewLExprEq in GHC.HsToCore.Match, which wants to group
-- together certain view patterns if they're "equal" in order to avoid multiple
-- evaluations of the same expression.

-- CSE might perform the same optimisation later, so we disable optimisations
{-# OPTIONS_GHC -Wall -O0 #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Exception
import Control.Monad
import Data.IORef
import System.IO.Unsafe
import Data.Functor

{-# NOINLINE calls #-}
calls :: IORef Int
calls = unsafePerformIO (newIORef 0)

{-# NOINLINE count #-}
count :: a -> a
count x = unsafePerformIO $ modifyIORef' calls (+1) $> x

data X = A | B

{-# NOINLINE view #-}
view :: a -> X
view x = count x `seq` B

{-# NOINLINE viewX #-}
viewX :: a -> Int -> X
viewX = const view

var, par, app, neg, neg', cond, tup :: Int -> Int

var (view -> A) = 1
var (view -> B) = 2

par (((view)) -> A) = 1
par (view -> B) = 2

app (viewX True -> A) = 1
app (viewX True -> B) = 2

neg (viewX @Int (-1) -> A) = 1
neg (viewX @Int (-1) -> B) = 2

type Int' = Int
neg' (viewX (-1 :: Int) -> A) = 1
neg' (viewX (-1 :: Int') -> B) = 2

cond (if True then view else undefined -> A) = 1
cond (if True then view else undefined -> B) = 2

tup (viewX (True, False) -> A) = 1
tup (viewX (True, False) -> B) = 2

check :: String -> Int -> IO ()
check name action = do
  writeIORef calls 0
  _ <- evaluate action
  got <- readIORef calls
  unless (got == 1) $ putStrLn $ name ++ ": got " ++ show got ++ ", expected 1"

main :: IO ()
main = do
  check "var" (var 0)
  check "par" (par 0)
  check "app" (app 0)
  check "neg" (neg 0)
  check "neg'" (neg' 0)
  check "cond" (cond 0)
  check "tup" (tup 0)

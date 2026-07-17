{-# LANGUAGE BangPatterns #-}

-- Keep the local functions local and their boxed results visible.
{-# OPTIONS_GHC -fno-cpr-anal -fno-full-laziness #-}

module Main where

import GHC.Exts (noinline)
import GHC.Exts.Heap (GenClosure(..), getClosureData)
import System.Exit (exitFailure)

data Box a = Box !a

recursive :: Int -> Int -> (Int, Box Int)
recursive !x n =
  let go y i
        | i == 0 = (i, Box y)
        | otherwise = go y (i - 1)
      {-# NOINLINE go #-}
  in go x n
{-# NOINLINE recursive #-}

mixed :: Bool -> Int -> Int -> (Box Int, Box Int)
mixed b !x z =
  let fun a c = (Box a, Box c)
      {-# NOINLINE fun #-}
  in if b then fun x x else fun x z
{-# NOINLINE mixed #-}

apply :: (a -> b) -> a -> b
apply fun x = fun x
{-# NOINLINE apply #-}

partial :: Int -> Int -> (Box Int, Box Int)
partial !x z =
  let fun a c = (Box a, Box c)
      {-# NOINLINE fun #-}
      pap = fun x
  in apply pap z
{-# NOINLINE partial #-}

escaping :: Int -> (Int, Box Int)
escaping !x =
  let fun a = (0, Box a)
      {-# NOINLINE fun #-}
  in case noinline Just fun of
       Just escaped -> escaped x
       Nothing -> (0, Box x)
{-# NOINLINE escaping #-}

checkConstr :: String -> a -> IO ()
checkConstr label value = do
  closure <- getClosureData value
  case closure of
    ConstrClosure{} -> pure ()
    _ -> putStrLn ("FAIL: " ++ label ++ " was not a constructor") >> exitFailure

checkThunk :: String -> a -> IO ()
checkThunk label value = do
  closure <- getClosureData value
  case closure of
    ThunkClosure{} -> pure ()
    _ -> putStrLn ("FAIL: " ++ label ++ " was not a thunk") >> exitFailure

main :: IO ()
main = do
  case recursive 42 1 of
    (_, value) -> checkConstr "recursive local function" value

  case mixed False 42 undefined of
    (known, unknown) -> do
      checkConstr "mixed known parameter" known
      checkThunk "mixed unknown parameter" unknown

  case partial 42 undefined of
    (known, unknown) -> do
      checkConstr "partial known prefix" known
      checkThunk "partial unknown suffix" unknown

  case escaping 42 of
    (_, value) -> checkThunk "escaping local function" value
  putStrLn "OK"

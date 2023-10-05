#!/usr/bin/env runghc
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

-- This little piece of code constructs a large set of functions
-- constructing and deconstructing unboxed tuples of various types.
module Main where

import GHC.Exts
import System.IO
import Data.List (intersperse)
inputs = ["Int", "Word"]
sizes = ["","8","16","32","64"]

-- ["Addr#","Int#","Int8#","Int16#","Int32#","Int64#","Word#","Word8#","Word16#","Word32#","Word64#"]
types = "Addr#" : do
    r <- inputs
    s <- sizes
    return $ r++s++"#"

-- We eventually build two sums, one of type (# t1 | t2 #) and one of (# t1 | t3).
-- So we build all possible combinations of three types here.
combos = do
    t1 <- types
    t2 <- types
    t3 <- types
    return (t1,t2,t3)

mkCon ty = case ty of
    "Addr#" -> "Addr"
    "Int#" -> "I#"
    "Int8#" -> "I8#"
    "Int16#" -> "I16#"
    "Int32#" -> "I32#"
    "Int64#" -> "I64#"
    "Word#" -> "W#"
    "Word8#" -> "W8#"
    "Word16#" -> "W16#"
    "Word32#" -> "W32#"
    "Word64#" -> "W64#"

-- Construct a function like the one below, varying the types in the sums based on the
-- given type tuples.
-- We need to NOINLINE or the function will be constant folded away.
-- {-# NOINLINE fun0 #-}
-- fun0 :: (# Addr# | I16# #) -> (# Addr# | I# #)
-- fun0 x = case x of
--   (# x1 | #) -> (# x1 | #) :: (# Addr# | I# #)
mkFun n (t1,t2,t3) =
    "{-# NOINLINE fun" ++ show n ++ " #-}\n" ++
    "fun" ++ show n ++ " :: (# " ++ t1 ++" | " ++ t2 ++ " #) -> (# " ++ t1 ++" | " ++ t3 ++ " #)\n" ++
    "fun" ++ show n ++ " x = case x of\n" ++
    "  (# x1 | #) -> (# x1 | #) :: (# " ++ t1 ++ " | " ++ t3 ++ " #)"

-- Generate functions for all the tuple combinations.
mkFuns _ [] = ""
mkFuns n (combo:combos) =
    mkFun n combo ++ "\n" ++ mkFuns (n+1) combos

-- generate a test that will put a value into a unboxed sum and then retrieve it later on.
-- It generates code like the one below:
-- test0 =
--     let in_val = maxBound
--         out_val = case in_val of I# x -> case fun0 (# x | #) of (# y | #) -> I# y
--     in in_val == out_val
mkTest n (t1,_,_)=
    let test_name = "test" ++ show n
        test_code = test_name ++ " =\n" ++
                    "  let in_val = (maxBound)\n" ++
                    "      out_val = case in_val of " ++ mkCon t1 ++ " x -> case fun" ++ show n ++ " (# x | #) of (# y | #) -> " ++ mkCon t1 ++ " y\n" ++
                    "  in in_val == out_val"
    in (test_code,test_name)

-- Test all the tuples
mkTests n combos =
    let (defs, names) = unzip $ zipWith mkTest [0..] combos
        assert_results = "\nassert_results = and [" ++ (concat $ intersperse "," names) ++ "]\n" :: String
    in unlines defs ++ assert_results

header =
    "{-# LANGUAGE MagicHash #-}\n\
    \{-# LANGUAGE UnboxedTuples #-}\n\
    \{-# LANGUAGE UnboxedSums #-}\n\
    \module Main where\n\
    \import GHC.Exts\n\
    \import GHC.Word\n\
    \import GHC.Int\n\
    \import ManyUbxSums_Addr\n"
main = do
    out <- openFile "ManyUbxSums.hs" WriteMode
    hPutStrLn out header

    let combo:_ = combos
    -- putStrLn $ mkFun 1 combo
    hPutStrLn out $ mkFuns 0 combos

    hPutStrLn out $ mkTests 0 combos
    hPutStrLn out "main = do"

    hPutStrLn out $ "  putStrLn . show $ assert_results"

    -- The snippet below would print all individual test results.
    -- But for CI really just check if all results match the input
    -- let runTest n =
    --         hPutStrLn out $ "  putStrLn $ \"test" ++ show n ++ " \" ++ (show test" ++ show n ++ ")"
    --         mapM runTest [0 .. length combos - 1]

    hClose out

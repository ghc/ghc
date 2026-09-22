{-# LANGUAGE MagicHash #-}
-- This test sanity checks the tag info we generate for imported binders which
-- have no real info in the .hi file.
--
-- This is important to avoid running into cases where we optimistically
-- generated better information based on things like type than the
-- compiled code actually provided. For example GHCi doesn't ensure tagging
-- of constructors.
module Main (main) where

import Control.Exception
import Control.Monad
import GHC.Exts (Addr#)
import GHC.Exts.Heap (GenClosure(..), getClosureData)
import GHC.Internal.Control.Exception.Base (patError)
import System.Exit

import qualified InferTags007_A as A

data S a = S !a

data P = MkP !(Addr# -> Int)

-- Put the imported values into a strict field of a top level Con
con_top_fun :: S (Int -> Int)
con_top_fun = S A.fun
{-# NOINLINE con_top_fun #-}

con_top_caf :: S [Int]
con_top_caf = S A.caf
{-# NOINLINE con_top_caf #-}

con_top_bot :: S (Int -> Int)
con_top_bot = S A.bot
{-# NOINLINE con_top_bot #-}

con_top_con :: S (Maybe Int)
con_top_con = S A.con
{-# NOINLINE con_top_con #-}

con_top_pat :: P
con_top_pat = MkP patError
{-# NOINLINE con_top_pat #-}

-- Put the imported values into a strict field of a Con inside a rhs
expr_fun :: () -> S (Int -> Int)
expr_fun _ = S A.fun
{-# OPAQUE expr_fun #-}

expr_caf :: () -> S [Int]
expr_caf _ = S A.caf
{-# OPAQUE expr_caf #-}

expr_bot :: () -> S (Int -> Int)
expr_bot _ = S A.bot
{-# OPAQUE expr_bot #-}

expr_con :: () -> S (Maybe Int)
expr_con _ = S A.con
{-# OPAQUE expr_con #-}

-- Actually use the field.
use_S_head :: S [Int] -> Int
use_S_head (S xs) = case xs of { (y:_) -> y; [] -> -1 }
{-# NOINLINE use_S_head #-}

use_S_fromJust :: S (Maybe Int) -> Int
use_S_fromJust (S m) = case m of { Just x -> x; Nothing -> -1 }
{-# NOINLINE use_S_fromJust #-}

use_S_apply :: S (Int -> Int) -> Int
use_S_apply (S f) = f 1
{-# NOINLINE use_S_apply #-}

checkNotThunk :: String -> a -> IO ()
checkNotThunk name x = do
    c <- getClosureData x
    case c of
        ThunkClosure{} -> putStrLn ("FAIL: " ++ name ++ " is a thunk") >> exitFailure
        APClosure{}    -> putStrLn ("FAIL: " ++ name ++ " is an AP") >> exitFailure
        _              -> pure ()

-- Pattern matching binds the field itself, so no selector thunk is involved.
checkField :: String -> S a -> IO ()
checkField name (S x) = checkNotThunk name x
{-# NOINLINE checkField #-}

main :: IO ()
main = do
    -- Check if the pointer in the strict field is a thunk.
    checkField "con_top_caf" con_top_caf
    checkField "con_top_fun" con_top_fun
    checkField "con_top_bot" con_top_bot
    checkField "con_top_con" con_top_con
    case con_top_pat of MkP f -> checkNotThunk "con_top_pat" f
    checkField "expr_caf" (expr_caf ())
    checkField "expr_fun" (expr_fun ())
    checkField "expr_bot" (expr_bot ())
    checkField "expr_con" (expr_con ())

    -- Actually execute a case on the contents of the field.
    print (use_S_head con_top_caf, use_S_head (expr_caf ()))
    print (use_S_fromJust con_top_con, use_S_fromJust (expr_con ()))
    print (use_S_apply con_top_fun, use_S_apply (expr_fun ()))
    forM_ [con_top_bot, expr_bot ()] $ \s -> do
        r <- try (evaluate (use_S_apply s))
        case r of
            Left (ErrorCall msg) -> putStrLn ("OK: " ++ msg)
            Right v              -> putStrLn ("FAIL: got " ++ show v) >> exitFailure

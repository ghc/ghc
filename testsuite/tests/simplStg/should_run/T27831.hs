-- Regression test for #27831: a bottoming function stored in a strict
-- constructor field.
--
-- Different parts of the tag inference machinery didn't agree on weither or not
-- a bottoming function should be treated as a EPT value, leading to a segfault.
module Main (main) where

import Control.Exception
import GHC.Exts.Heap (GenClosure(..), getClosureData)
import System.Exit

data T = MkT Int !(Int -> Int)

t :: T
t = MkT 1 (\_ -> error "T27831")
{-# NOINLINE t #-}

f :: () -> T
f _ = t
{-# OPAQUE f #-}

main :: IO ()
main = do
    c <- getClosureData t
    case c of
        ThunkClosure{} -> putStrLn "FAIL: ThunkClosure" >> exitFailure
        APClosure{}    -> putStrLn "FAIL: APClosure" >> exitFailure
        _              -> putStrLn "OK: not a thunk"

    case f () of MkT n _ -> print n

    r <- try (evaluate (case t of MkT _ g -> g 1))
    case r of
        Left (ErrorCall msg) -> putStrLn ("OK: " ++ msg)
        Right v              -> putStrLn ("FAIL: got " ++ show v) >> exitFailure

{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import GHC.Exts
import GHC.Exts.HeapView
import Control.DeepSeq

import System.Environment
import System.Mem

import Control.Monad

main :: IO ()
main = do
    args <- map length `fmap` getArgs
    let list2 = 4:list
    (list ++ list2 ++ args) `deepseq` pure ()

    let x = list ++ list2 ++ args
    performGC
    getClosureAssert list >>= \ cl ->
        unless (name cl == ":") $ fail "Wrong name"

    getClosureAssert list2 >>= \ cl -> do
        eq <- areBoxesEqual (ptrArgs cl !! 1) (asBox list)
        unless eq $ fail "Doesn't reference list"

    getClosureData args >>= \ cl ->
        unless (cltype (info cl) == CONSTR_0_1) $
            fail $ "Not a CONSTR_0_1"

    getClosureData x >>= \ cl ->
        unless (cltype (info cl) == THUNK_2_0) $ do
            fail "Not a THUNK_2_0"


    let !(I# m) = length args + 42
    let !(I# m') = length args + 23
    let f = \ y n -> take (I# m + I# y) n ++ args
        t = f m' list2

    getClosureData f >>= \ cl -> do
        unless (cltype (info cl) == FUN_1_1) $ do
            fail "Not a FUN_1_1"
        unless (dataArgs cl == [42]) $ do
            fail "Wrong data arg"

    getClosureData t >>= \ cl -> do
        unless (cltype (info cl) == THUNK) $ do
            fail "Not a THUNK"
        unless (dataArgs cl == [23]) $ do
            fail "Wrong data arg"

        eq <- areBoxesEqual (ptrArgs cl !! 1) (asBox f)
        unless eq $ fail "t doesnt reference f"

    let z = id (:) () z
    z `seq` pure ()
    performGC
    getClosureAssert z >>= \ cl -> do
        eq <- areBoxesEqual (ptrArgs cl !! 1) (asBox z)
        unless eq $
            fail "z doesnt reference itself"

    putStrLn "Done. No errors."


list :: [Int]
list = [1,2,3]


getClosureAssert :: a -> IO Closure
getClosureAssert x = do
    cl <- getClosureData x
    case cl of
        ConsClosure {} -> pure cl
        _ -> fail "Expected ConsClosure"

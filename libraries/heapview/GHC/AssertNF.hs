{-# LANGUAGE BangPatterns, DoAndIfThenElse #-}

{-|
Module      :  GHC.AssertNF
Copyright   :  (c) 2013 Joachim Breitner
License     :  BSD3
Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>

To avoid space leaks and unwanted evaluation behaviour, the programmer might want his data to be fully evaluated at certain positions in the code. This can be enforced, for example, by ample use of "Control.DeepSeq", but this comes at a cost.

Experienced users hence use 'Control.DeepSeq.deepseq' only to find out about the existence of space leaks and optimize their code to not create the thunks in the first place, until the code no longer shows better performance with 'deepseq'.

This module provides an alternative approach: An explicit assertion about the evaluation state. If the programmer expect a certain value to be fully evaluated at a specific point of the program (e.g. before a call to 'writeIORef'), he can state that, and as long as assertions are enabled, this statement will be checked. In the production code the assertions can be disabled, to avoid the run-time cost.

-}


module GHC.AssertNF (
    assertNF,
    assertNFNamed,
    assertNFHere,
    disableAssertNF,
    isNF,
    )
where

import GHC.HeapView
import Debug.Trace
import Control.Monad
import Text.Printf
import Language.Haskell.TH (Q, Exp(AppE,VarE,LitE), Lit(StringL), Loc, location, loc_filename, loc_start, mkName)
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

enabledRef :: IORef Bool
enabledRef = unsafePerformIO $ newIORef True
{-# NOINLINE enabledRef #-}

-- Everything is in normal form, unless it is a
-- thunk explicitly marked as such.
-- Indirection are also considered to be in HNF
isHNF :: Closure -> IO Bool
isHNF c = do
    case c of
        ThunkClosure {}    -> return False
        APClosure {}       -> return False
        SelectorClosure {} -> return False
        BCOClosure {}      -> return False
        _                  -> return True

-- | The function 'assertNF' checks whether its argument is fully evaluated and
-- deeply evaluated. If this is not the case, a warning is printed to the standard output,
-- giving the number of thunks found and printing the shape of the unevaluated object:
--
-- >> let x = 1 + 2
-- >> let y = (x,x)
-- >> assertNF y
-- >Parameter not in normal form: 2 thunks found:
-- >let t1 = _bco
-- >in (t1,t1)
-- >> x
-- >3
-- >> assertNF y
-- >>
--
assertNF :: a -> IO ()
assertNF = assertNF' "Parameter not in normal form"

-- | In order to better identify the source of error messages from 'assertNF', this variant allows you to include a name that is printed in the output:
--
-- >> assertNFNamed "y" y
-- >y not in normal form: 2 thunks found:
-- >let t1 = _bco
-- >in (t1,t1)
--
assertNFNamed :: String -> a -> IO ()
assertNFNamed valName = assertNF' (valName ++ " not in normal form")

-- | This function, when called as @$assertNFHere@ in a module with @-XTemplateHaskell@ enabled, will cause the current filename and position be included in the error message:
--
-- >Parameter at Test.hs:18:1 not in normal form: 2 thunks found:
-- >let t1 = _bco
-- >in (t1,t1)
--
assertNFHere :: Q Exp
assertNFHere = do
    locStr <- formatLoc <$> location
    -- We don't use ''assertNF here, so that this module can be used on a
    -- compiler that does not support TH.
    return $ AppE (VarE (mkName "GHC.AssertNF.assertNFNamed"))
                  (LitE (StringL locStr))
  where formatLoc :: Loc -> String
        formatLoc loc = let file = loc_filename loc
                            (line, col) = loc_start loc
                        in  printf "parameter at %s:%d:%d" file line col

assertNF' :: String ->  a -> IO ()
assertNF' str x = do
    en <- readIORef enabledRef
    when en $ do
        depths <- assertNFBoxed 0 (asBox x)
        unless (null depths) $ do
            g <- buildHeapGraph (maximum depths + 3) () (asBox x)
                -- +3 for good mesure; applications don't look good otherwise
            traceIO $ str ++ ": " ++ show (length depths) ++ " thunks found:\n" ++
                ppHeapGraph g


assertNFBoxed :: Int -> Box -> IO [Int]
assertNFBoxed !d b = do
    c <- getBoxedClosureData b
    nf <- isHNF c
    if nf
    then do
        c' <- getBoxedClosureData b
        concat <$> mapM (assertNFBoxed (d+1)) (allPtrs c')
    else do
        return [d]

-- | Invoke this function at the top of your 'main' method to turn every call
-- to 'assertNF' and its variants to noops.
disableAssertNF :: IO ()
disableAssertNF = writeIORef enabledRef False

-- | A variant of 'assertNF' that does not print anything and just returns
-- 'True' if the value is in normal form, or 'False' otherwise. This function
-- is not affected by 'disableAssertNF'.
isNF :: a -> IO Bool
isNF x = isNFBoxed (asBox x)

isNFBoxed :: Box -> IO Bool
isNFBoxed b = do
    c <- getBoxedClosureData b
    nf <- isHNF c
    if nf
    then do
        c' <- getBoxedClosureData b
        allM isNFBoxed (allPtrs c')
    else do
        return False

-- From Control.Monad.Loops in monad-loops, but I'd like to avoid too many
-- trivial dependencies
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
        q <- p x
        if q
                then allM p xs
                else return False

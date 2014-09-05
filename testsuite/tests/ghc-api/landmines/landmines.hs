{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import System.IO
import GHC
import MonadUtils
import Outputable
import Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "MineFixity"
        testOneFile libdir "MineKind"
        testOneFile libdir "MineNames"
        testOneFile libdir "MineType"


testOneFile libdir fileName = do
        (p,r,ts) <- runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        setSessionDynFlags dflags
                        let mn =mkModuleName fileName
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        load LoadAllTargets
                        modSum <- getModSummary mn
                        p <- parseModule modSum
                        t <- typecheckModule p
                        d <- desugarModule t
                        l <- loadModule d
                        let ts=typecheckedSource l
                            r =renamedSource l
                        -- liftIO (putStr (showSDocDebug (ppr ts)))
                        return (pm_parsed_source p,r,ts)
        let pCount = gq p
            rCount = gq r
            tsCount = gq ts

        print (pCount,rCount,tsCount)
    where
        gq ast = length $ everything (++)    ([] `mkQ` worker) ast

        worker (s@(RealSrcSpan _)) = [s]
        worker _ = []

-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r



-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results

everything k f x = foldl k (f x) (gmapQ (everything k f) x)

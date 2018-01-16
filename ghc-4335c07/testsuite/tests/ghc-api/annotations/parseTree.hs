{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import Data.List
import System.IO
import GHC
import BasicTypes
import DynFlags
import MonadUtils
import Outputable
import Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "AnnotationTuple"

testOneFile libdir fileName = do
       ((anns,cs),p) <- runGhc (Just libdir) $ do
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
                        return (pm_annotations p,p)

       let tupArgs = gq (pm_parsed_source p)

       putStrLn (pp tupArgs)
       putStrLn (intercalate "\n" [showAnns anns])

    where
     gq ast = everything (++) ([] `mkQ` doLHsTupArg) ast

     doLHsTupArg :: LHsTupArg GhcPs -> [(SrcSpan,String,HsExpr GhcPs)]
     doLHsTupArg (L l arg@(Present _)) = [(l,"p",ExplicitTuple [L l arg] Boxed)]
     doLHsTupArg (L l arg@(Missing _)) = [(l,"m",ExplicitTuple [L l arg] Boxed)]


showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\((s,k),v)
              -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n"))
   $ Map.toList anns)
    ++ "]\n"

pp a = showPpr unsafeGlobalDynFlags a


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

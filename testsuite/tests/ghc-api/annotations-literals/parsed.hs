{-# LANGUAGE RankNTypes #-}
-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

import BasicTypes
import Data.Data
import Data.List
import System.IO
import GHC
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
        testOneFile libdir "LiteralsTest2"

testOneFile libdir fileName = do
    p <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        let mn =mkModuleName fileName
        addTarget Target { targetId = TargetModule mn
                         , targetAllowObjCode = True
                         , targetContents = Nothing }
        load LoadAllTargets
        modSum <- getModSummary mn
        p <- GHC.parseModule modSum
        return p

    let res = gq (pm_parsed_source p)
    putStrLn (intercalate "\n" res)

    where
     gq ast = everything (++) ([] `mkQ` doHsLit `extQ` doOverLit) ast

     doHsLit :: HsLit GhcPs -> [String]
     doHsLit (HsChar       (SourceText src) c)
       = ["HsChar [" ++ src ++ "] " ++ show c]
     doHsLit (HsCharPrim   (SourceText src) c)
       = ["HsCharPrim [" ++ src ++ "] " ++ show c]
     doHsLit (HsString     (SourceText src) c)
       = ["HsString [" ++ src ++ "] " ++ show c]
     doHsLit (HsStringPrim (SourceText src) c)
       = ["HsStringPrim [" ++ src ++ "] " ++ show c]
     doHsLit (HsInt  _     (IL (SourceText src) _ c))
       = ["HsInt [" ++ src ++ "] " ++ show c]
     doHsLit (HsIntPrim (SourceText src) c)
       = ["HsIntPrim [" ++ src ++ "] " ++ show c]
     doHsLit (HsWordPrim   (SourceText src) c)
       = ["HsWordPrim [" ++ src ++ "] " ++ show c]
     doHsLit (HsInt64Prim  (SourceText src) c)
       = ["HsInt64Prim [" ++ src ++ "] " ++ show c]
     doHsLit (HsWord64Prim (SourceText src) c)
       = ["HsWord64Prim [" ++ src ++ "] " ++ show c]
     doHsLit (HsInteger  (SourceText src) c _)
       = ["HsInteger [" ++ src ++ "] " ++ show c]
     doHsLit _ = []

     doOverLit :: OverLitVal -> [String]
     doOverLit (HsIntegral  (IL (SourceText src) _ c))
       = ["HsIntegral [" ++ src ++ "] " ++ show c]
     doOverLit (HsIsString  (SourceText src) c)
       = ["HsIsString [" ++ src ++ "] " ++ show c]
     doOverLit _ = []

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

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)



-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results

everything k f x = foldl k (f x) (gmapQ (everything k f) x)

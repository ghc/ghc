{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import FastString
import ForeignCall
import MonadUtils
import Outputable
import HsDecls
import Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir,fileName] <- getArgs
        testOneFile libdir fileName

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
                        return (pm_annotations p,p)

       let tupArgs = gq (pm_parsed_source p)

       putStrLn (pp tupArgs)
       -- putStrLn (intercalate "\n" [showAnns anns])

    where
     gq ast = everything (++) ([] `mkQ` doWarningTxt
                               `extQ` doImportDecl
                               `extQ` doCType
                               `extQ` doRuleDecl
                               `extQ` doCCallTarget
                               `extQ` doHsExpr
                              ) ast

     doWarningTxt :: WarningTxt -> [(String,[Located (SourceText,FastString)])]
     doWarningTxt ((WarningTxt _ ss))    = [("w",ss)]
     doWarningTxt ((DeprecatedTxt _ ss)) = [("d",ss)]

     doImportDecl :: ImportDecl RdrName
                  -> [(String,[Located (SourceText,FastString)])]
     doImportDecl (ImportDecl _ _ Nothing _ _ _ _ _ _) = []
     doImportDecl (ImportDecl _ _ (Just ss) _ _ _ _ _ _) = [("i",[noLoc ss])]

     doCType :: CType -> [(String,[Located (SourceText,FastString)])]
     doCType (CType src (Just (Header hs hf)) c)
                                    = [("c",[noLoc (hs,hf),noLoc c])]
     doCType (CType src Nothing  c) = [("c",[noLoc c])]

     doRuleDecl :: RuleDecl RdrName
                -> [(String,[Located (SourceText,FastString)])]
     doRuleDecl (HsRule ss _ _ _ _ _ _) = [("r",[ss])]

     doCCallTarget :: CCallTarget
                   -> [(String,[Located (SourceText,FastString)])]
     doCCallTarget (StaticTarget s f _ _) = [("st",[(noLoc (s,f))])]

     doHsExpr :: HsExpr RdrName -> [(String,[Located (SourceText,FastString)])]
     doHsExpr (HsCoreAnn src ss _) = [("co",[noLoc ss])]
     doHsExpr (HsSCC     src ss _) = [("sc",[noLoc ss])]
     doHsExpr (HsTickPragma src (ss,_,_) _) = [("tp",[noLoc ss])]
     doHsExpr _ = []

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

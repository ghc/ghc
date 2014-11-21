{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
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
        testOneFile libdir "CommentsTest" True
        testOneFile libdir "CommentsTest" False

testOneFile libdir fileName useHaddock = do
    p <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = if useHaddock
                        then gopt_set (gopt_set dflags Opt_Haddock)
                                       Opt_KeepRawTokenStream
                        else gopt_set (gopt_unset dflags Opt_Haddock)
                                       Opt_KeepRawTokenStream
        setSessionDynFlags dflags'
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
        return (pm_annotations p)

    let anns = p

    putStrLn (intercalate "\n" [showAnns anns])

showAnns (_,anns) = "[\n" ++ (intercalate "\n"
   $ map (\(s,v)
              -> ("( " ++ pp s ++" =\n[" ++ showToks v ++ "])\n"))
   $ Map.toList anns)
    ++ "]\n"

showToks ts = intercalate ",\n\n"
            $ map (\(L p t) -> "(" ++ pp p ++ "," ++ show t ++ ")") ts

pp a = showPpr unsafeGlobalDynFlags a

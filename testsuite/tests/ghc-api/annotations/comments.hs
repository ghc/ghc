{-# LANGUAGE RankNTypes #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import Data.List (intercalate)
import System.IO
import GHC
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Data.Bag (filterBag,isEmptyBag)
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
        -- ann_comments = apiAnnComments anns
        ann_comments = Map.empty
        ann_rcomments = apiAnnRogueComments anns
        comments =
          map (\(s,v) -> (RealSrcSpan s Nothing, v)) (Map.toList ann_comments)
            ++
          [(noSrcSpan, ann_rcomments)]

    putStrLn (intercalate "\n" [showAnns comments])

showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\(s,v)
              -> ("( " ++ pp s ++" =\n[" ++ showToks v ++ "])\n"))
   $ anns)
    ++ "]\n"

showToks ts = intercalate ",\n\n"
            $ map (\(L p t) -> "(" ++ pp p ++ "," ++ show t ++ ")") ts

pp a = showPprUnsafe a

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Main where

import System.IO
import GHC
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Data.Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )

main::IO()
main = do
        let c="module Test where\ndata DataT=MkData {name :: String}\n"
        writeFile "Test.hs" c
        [libdir] <- getArgs
        ok<-    runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        setSessionDynFlags dflags
                        let mn =mkModuleName "Test"
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetUnitId = homeUnitId_ dflags
                                         , targetContents = Nothing}
                        load LoadAllTargets
                        modSum <- getModSummary mn
                        p <- parseModule modSum
                        t <- typecheckModule p
                        d <- desugarModule t
                        let ts=typecheckedSource d
--                        liftIO (putStr (showSDocDebug (ppr ts)))
                        let fs=filterBag isDataCon ts
                        return $ not $ isEmptyBag fs
        removeFile "Test.hs"
        print ok
    where
      isDataCon (L _ (AbsBinds { abs_binds = bs }))
        = not (isEmptyBag (filterBag isDataCon bs))
      isDataCon (L l (f@FunBind {}))
        | (MG _ (L _ (m:_)) _) <- fun_matches f,
          ((L _ (VisPat _ (L _ (c@ConPat{})))):_)<-hsLMatchPats m,
          (L l _)<-pat_con c
        = isGoodSrcSpan (locA l) -- Check that the source location is a good one
      isDataCon _
        = False

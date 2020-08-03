{-# LANGUAGE TemplateHaskell, ExplicitForAll, PolyKinds #-}

module Main where

import Language.Haskell.TH (runQ)
import GHC.Types.Basic
import GHC.ThToHs
import GHC.Driver.Session
import GHC.Core.TyCo.Ppr
import GHC.Utils.Outputable
import GHC.Tc.Module
import GHC.Tc.Utils.Zonk
import GHC.Utils.Error
import GHC.Driver.Types
import GHC
import qualified GHC.LanguageExtensions as LangExt

import Data.Either (fromRight)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    initial_dflags <- getSessionDynFlags
    setSessionDynFlags $ initial_dflags
      `dopt_set` Opt_D_ppr_debug
      `gopt_set` Opt_SuppressUniques
      `gopt_set` Opt_SuppressModulePrefixes
      `gopt_set` Opt_SuppressVarKinds
      `xopt_set` LangExt.KindSignatures
      `xopt_set` LangExt.PolyKinds
      `xopt_set` LangExt.RankNTypes
    hsc_env <- getSession
    let dflags = hsc_dflags hsc_env
    liftIO $ do
      th_t <- runQ [t| forall k {j}.
                       forall (a :: k) (b :: j) ->
                       () |]
      let hs_t = fromRight (error "convertToHsType") $
                 convertToHsType Generated noSrcSpan th_t
      ((warnings, errors), mres) <-
        tcRnType hsc_env SkolemiseFlexi True hs_t
      case mres of
        Nothing -> do
          printBagOfErrors dflags warnings
          printBagOfErrors dflags errors
        Just (t, _) -> do
          putStrLn $ showSDoc dflags (debugPprType t)

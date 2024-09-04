{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LoadCmmGroup
  ( loadPath
  , loadCmm
  , loadHs
  )
where

-- Read a .hs or .cmm file and convert it to a list of `CmmGroup`s.

import Control.Monad.IO.Class
import System.FilePath as FilePath
import System.IO

import GHC
import GHC.Cmm
import GHC.Cmm.Parser
import GHC.Core.Lint.Interactive
import GHC.Core.TyCon
import GHC.CoreToStg
import GHC.CoreToStg.Prep
import GHC.Data.Stream hiding (mapM, map)
import GHC.Driver.Config.Cmm.Parser (initCmmParserConfig)
import GHC.Driver.Config.CoreToStg
import GHC.Driver.Config.CoreToStg.Prep
import GHC.Driver.Config.StgToCmm (initStgToCmmConfig)
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Stg.FVs
import GHC.Stg.Syntax
import GHC.StgToCmm (codeGen)
import GHC.Types.CostCentre (emptyCollectedCCs)
import GHC.Types.HpcInfo (emptyHpcInfo)
import GHC.Types.IPE (emptyInfoTableProvMap)
import GHC.Types.Unique.DSM
import GHC.Unit.Home
import GHC.Unit.Module.ModGuts
import GHC.Utils.Error
import GHC.Utils.Misc (fstOf3)
import GHC.Utils.Outputable


loadPath :: FilePath -> Ghc [CmmGroup]
loadPath path =
    case takeExtension path of
      ".hs" -> loadHs path
      ".cmm" -> fmap (: []) $ loadCmm path
      _ -> do liftIO $ hPutStrLn stderr $ "File with unknown extension: " ++ path
              return []

loadHs :: FilePath -> Ghc [CmmGroup]
loadHs path = do
  target <- guessTarget path Nothing Nothing
  setTargets [target]
  mgraph <- depanal [] False
  fmap concat $ mapM cmmOfSummary $ mgModSummaries mgraph

cmmOfSummary :: ModSummary -> GHC.Ghc [CmmGroup]
cmmOfSummary summ = do
  dflags <- getSessionDynFlags
  env <- getSession
  guts <- liftIO $ frontend dflags env summ
  stg <- stgify summ guts
  logger <- getLogger
  let infotable = emptyInfoTableProvMap
      tycons = []
      ccs = emptyCollectedCCs
      stg' = fmap fst (depSortWithAnnotStgPgm (ms_mod summ) stg)
      hpcinfo = emptyHpcInfo False
      tmpfs = hsc_tmpfs env
      stg_to_cmm dflags mod = codeGen logger tmpfs (initStgToCmmConfig dflags mod)
  (groups, _infos) <-
      liftIO $ fmap fst $
      runUDSMT (initDUniqSupply 't' 0) $
      collectAll $
      stg_to_cmm dflags (ms_mod summ) infotable tycons ccs stg' hpcinfo
  return groups

frontend :: DynFlags -> HscEnv -> ModSummary -> IO ModGuts
frontend _dflags env summary = do
   parsed <- hscParse env summary
   (checked, _) <- hscTypecheckRename env summary parsed
   hscDesugar env summary checked >>= hscSimplify env []

loadCmm :: FilePath -> Ghc CmmGroup
loadCmm path = do
  env <- getSession
  liftIO (slurpCmm env path)

stgify :: ModSummary -> ModGuts -> Ghc [StgTopBinding]
stgify summary guts = do
    hsc_env <- getSession
    let dflags = hsc_dflags hsc_env
    prepd_binds <- liftIO $ do
      cp_cfg <- initCorePrepConfig hsc_env
      corePrepPgm (hsc_logger hsc_env) cp_cfg (initCorePrepPgmConfig dflags (interactiveInScope $ hsc_IC hsc_env)) this_mod location core_binds data_tycons
    return $ fstOf3 $ coreToStg (initCoreToStgOpts dflags) (ms_mod summary) (ms_location summary) prepd_binds
  where this_mod = mg_module guts
        location = ms_location summary
        core_binds = mg_binds guts
        data_tycons = filter isDataTyCon tycons
        tycons = mg_tcs guts


slurpCmm :: HscEnv -> FilePath -> IO (CmmGroup)
slurpCmm hsc_env filename = runHsc hsc_env $ do
    let dflags   = hsc_dflags hsc_env
    let logger   = hsc_logger hsc_env
    let home_unit = hsc_home_unit hsc_env
        -- Make up a module name to give the NCG. We can't pass bottom here
        -- lest we reproduce #11784.
        mod_name = mkModuleName $ "Cmm$" ++ FilePath.takeFileName filename
        cmm_mod = mkHomeModule home_unit mod_name
        cmmpConfig = initCmmParserConfig dflags
    (cmm, _) <- ioMsgMaybe
               $ do
                  (warns,errs,cmm) <- withTiming logger (text "ParseCmm"<+>brackets (text filename)) (\_ -> ())
                                       $ parseCmmFile cmmpConfig cmm_mod home_unit filename
                  let msgs = warns `unionMessages` errs
                  return (GhcPsMessage <$> msgs, cmm)
    return (removeDeterm cmm)

collectAll :: Monad m => Stream m a b -> m ([a], b)
collectAll = gobble . runStream
    where gobble (Done b) = return ([], b)
          gobble (Effect e) = e >>= gobble
          gobble (Yield a s) = do (as, b) <- gobble s
                                  return (a:as, b)

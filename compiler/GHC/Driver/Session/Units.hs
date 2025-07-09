{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TupleSections #-}
module GHC.Driver.Session.Units (initMake, initMulti) where

import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Phases
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Pipeline  ( oneShot, compileFile )
import GHC.Driver.Config.Diagnostic

import GHC.Unit.Env
import GHC.Unit (UnitId)
import GHC.Unit.Home.PackageTable
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.State  ( emptyUnitState )
import qualified GHC.Unit.State as State

import GHC.Types.SrcLoc
import GHC.Types.SourceError

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Monad       ( liftIO, mapMaybeM )
import GHC.Data.Maybe

import System.IO
import System.Exit
import System.FilePath
import Control.Monad
import Data.List ( partition, (\\) )
import qualified Data.Set as Set
import Prelude
import GHC.ResponseFile (expandResponse)
import Data.Bifunctor
import GHC.Data.Graph.Directed
import qualified Data.List.NonEmpty as NE

-- Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
removeRTS :: [String] -> [String]
removeRTS ("+RTS" : xs)  =
  case dropWhile (/= "-RTS") xs of
    [] -> []
    (_ : ys) -> removeRTS ys
removeRTS (y:ys)         = y : removeRTS ys
removeRTS []             = []

initMake :: [(String,Maybe Phase)] -> Ghc [(String, Maybe Phase)]
initMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env NoStop srcs) >> return []
       else do

    o_files <- mapMaybeM (\x -> liftIO $ compileFile hsc_env NoStop x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'
    return hs_srcs

initMulti :: NE.NonEmpty String
          -> (DynFlags -> [(String,Maybe Phase)] -> [String] -> [String] -> IO ())
          -- ^ Function to lint initMulti DynFlags and sources.
          -- In GHC, this is instanced to @checkOptions@.
          -> Ghc ([(String, Maybe UnitId, Maybe Phase)])
initMulti unitArgsFiles lintDynFlagsAndSrcs = do
  hsc_env <- GHC.getSession
  let logger = hsc_logger hsc_env
  initial_dflags <- GHC.getSessionDynFlags

  dynFlagsAndSrcs <- forM unitArgsFiles $ \f -> do
    when (verbosity initial_dflags > 2) (liftIO $ print f)
    args <- liftIO $ expandResponse [f]
    (dflags2, fileish_args, warns) <- parseDynamicFlagsCmdLine logger initial_dflags (map (mkGeneralLocated f) (removeRTS args))
    handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)

    let (dflags3, srcs, objs) = parseTargetFiles dflags2 (map unLoc fileish_args)
        dflags4 = offsetDynFlags dflags3

    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    -- This is dubious as the whole unit environment won't be set-up correctly, but
    -- that doesn't matter for what we use it for (linking and oneShot)
    let dubious_hsc_env = hscSetFlags dflags4 hsc_env
    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot dubious_hsc_env NoStop srcs) >> return (dflags4, [])
       else do

    o_files <- mapMaybeM (\x -> liftIO $ compileFile dubious_hsc_env NoStop x)
                 non_hs_srcs
    let dflags5 = dflags4 { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags4 }

    liftIO $ lintDynFlagsAndSrcs dflags5 srcs objs []

    pure (dflags5, hs_srcs)

  let
    unitDflags = NE.map fst dynFlagsAndSrcs
    srcs = NE.map (\(dflags, lsrcs) -> map (uncurry (,Just $ homeUnitId_ dflags,)) lsrcs) dynFlagsAndSrcs
    (hs_srcs, _non_hs_srcs) = unzip (map (partition (\(file, _uid, phase) -> isHaskellishTarget (file, phase))) (NE.toList srcs))

  checkDuplicateUnits initial_dflags (NE.toList (NE.zip unitArgsFiles unitDflags))

  (initial_home_graph, mainUnitId) <- liftIO $ createUnitEnvFromFlags unitDflags
  let home_units = HUG.allUnits initial_home_graph

  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        hue_flags = homeUnitEnv_dflags homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
    (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits logger hue_flags cached_unit_dbs home_units

    updated_dflags <- liftIO $ updatePlatformConstants dflags mconstants
    emptyHpt <- liftIO $ emptyHomePackageTable
    pure $ HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = emptyHpt
      , homeUnitEnv_home_unit = Just home_unit
      }

  checkUnitCycles initial_dflags home_unit_graph

  let dflags = homeUnitEnv_dflags $ HUG.unitEnv_lookup mainUnitId home_unit_graph
  unitEnv <- assertUnitEnvInvariant <$> (liftIO $ initUnitEnv mainUnitId home_unit_graph (ghcNameVersion dflags) (targetPlatform dflags))
  let final_hsc_env = hsc_env { hsc_unit_env = unitEnv }

  GHC.setSession final_hsc_env

  -- if we have no haskell sources from which to do a dependency
  -- analysis, then just do one-shot compilation and/or linking.
  -- This means that "ghc Foo.o Bar.o -o baz" links the program as
  -- we expect.
  if (null hs_srcs)
      then do
        liftIO $ hPutStrLn stderr $ "Multi Mode can not be used for one-shot mode."
        liftIO $ exitWith (ExitFailure 1)
      else do

{-
  o_files <- liftIO $ mapMaybeM
                (\(src, uid, mphase) ->
                  compileFile (hscSetActiveHomeUnit (ue_unitHomeUnit (fromJust uid) unitEnv) final_hsc_env) NoStop (src, mphase)
                )
                (concat non_hs_srcs)
                -}

  -- MP: This should probably modify dflags for each unit?
  --let dflags' = dflags { ldInputs = map (FileOption "") o_files
  --                                  ++ ldInputs dflags }
  return $ concat hs_srcs

checkUnitCycles :: DynFlags -> HUG.HomeUnitGraph -> Ghc ()
checkUnitCycles dflags graph = processSCCs (HUG.hugSCCs graph)
  where

    processSCCs [] = return ()
    processSCCs (AcyclicSCC _: other_sccs) = processSCCs other_sccs
    processSCCs (CyclicSCC uids: _) = throwGhcException $ CmdLineError $ showSDoc dflags (cycle_err uids)


    cycle_err uids =
      hang (text "Units form a dependency cycle:")
           2
           (one_err uids)

    one_err uids = vcat $
                    (map (\uid -> text "-" <+> ppr uid <+> text "depends on") start)
                    ++ [text "-" <+> ppr final]
      where
        start = init uids
        final = last uids

-- | Check that we don't have multiple units with the same UnitId.
checkDuplicateUnits :: DynFlags -> [(FilePath, DynFlags)] -> Ghc ()
checkDuplicateUnits dflags flags =
  unless (null duplicate_ids)
         (throwGhcException $ CmdLineError $ showSDoc dflags multi_err)

  where
    uids = map (second homeUnitId_) flags
    deduplicated_uids = ordNubOn snd uids
    duplicate_ids = Set.fromList (map snd uids \\ map snd deduplicated_uids)

    duplicate_flags = filter (flip Set.member duplicate_ids . snd) uids

    one_err (fp, home_uid) = text "-" <+> ppr home_uid <+> text "defined in" <+> text fp

    multi_err =
      hang (text "Multiple units with the same unit-id:")
           2
           (vcat (map one_err duplicate_flags))


offsetDynFlags :: DynFlags -> DynFlags
offsetDynFlags dflags =
  dflags { hiDir = c hiDir
         , objectDir  = c objectDir
         , stubDir = c stubDir
         , hieDir  = c hieDir
         , dumpDir = c dumpDir  }

  where
    c f = augment_maybe (f dflags)

    augment_maybe Nothing = Nothing
    augment_maybe (Just f) = Just (augment f)
    augment f | isRelative f, Just offset <- workingDirectory dflags = offset </> f
              | otherwise = f


createUnitEnvFromFlags :: NE.NonEmpty DynFlags -> IO (HomeUnitGraph, UnitId)
createUnitEnvFromFlags unitDflags = do
  unitEnvList <- forM unitDflags $ \dflags -> do
    emptyHpt <- emptyHomePackageTable
    let newInternalUnitEnv =
          HUG.mkHomeUnitEnv emptyUnitState Nothing dflags emptyHpt Nothing
    return (homeUnitId_ dflags, newInternalUnitEnv)
  let activeUnit = fst $ NE.head unitEnvList
  return (HUG.hugFromList (NE.toList unitEnvList), activeUnit)



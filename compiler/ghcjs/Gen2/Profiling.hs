{-# LANGUAGE OverloadedStrings #-}

module Gen2.Profiling
  ( module CostCentre
  , initCostCentres
  , costCentreLbl
  , costCentreStackLbl
  , ccsVarJ
  , ifProfiling
  , ifProfiling'
  , ifProfilingM
  , profiling
  , profStat
  , setCC
  , pushRestoreCCS
  , jCurrentCCS
  , jCafCCS
  , jSystemCCS
  , enterCostCentreFun
  , enterCostCentreThunk
  ) where


import           CostCentre
import           Encoding
import           Module
import           Outputable           hiding ((<>))

-- import           Control.Lens (use)

import qualified Data.Text            as T

import           Compiler.JMacro
import           Compiler.JMacro.Lens
import           Compiler.JMacro.Combinators
import           Compiler.JMacro.Symbols

import           Gen2.ClosureInfo
import           Gen2.RtsTypes
import           Gen2.Utils
import Prelude

--------------------------------------------------------------------------------
-- Initialization

initCostCentres :: CollectedCCs -> G ()
initCostCentres (local_CCs, singleton_CCSs) = do
    mapM_ emitCostCentreDecl local_CCs
    mapM_ emitCostCentreStackDecl singleton_CCSs

emitCostCentreDecl :: CostCentre -> G ()
emitCostCentreDecl cc = do
    dflags <- use gsDynFlags
    ccsLbl <- costCentreLbl cc
    let is_caf = isCafCC cc
        label  = costCentreUserName cc
        modl   = Module.moduleNameString $ moduleName $ cc_mod cc
        loc    = showPpr dflags (costCentreSrcSpan cc)
        js     = ccsLbl ||= UOpExpr NewOp (app "h$CC" [e label, e modl, e loc, e is_caf])

    emitGlobal js

emitCostCentreStackDecl :: CostCentreStack -> G ()
emitCostCentreStackDecl ccs =
    case maybeSingletonCCS ccs of
      Just cc -> do
        ccsLbl <- singletonCCSLbl cc
        ccLbl  <- costCentreLbl cc
        let js = ccsLbl ||= UOpExpr NewOp (app "h$CCS" [null_, e ccLbl])
        emitGlobal js
      Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

--------------------------------------------------------------------------------
-- Entering to cost-centres

enterCostCentreFun :: CostCentreStack -> JStat
enterCostCentreFun ccs
  | isCurrentCCS ccs = appS "h$enterFunCCS" [jCurrentCCS, r1 .^ "cc"]
  | otherwise = mempty -- top-level function, nothing to do

enterCostCentreThunk :: JStat
enterCostCentreThunk = appS "h$enterThunkCCS" [r1 .^ "cc"]

setCC :: CostCentre -> Bool -> Bool -> G JStat
-- FIXME: ignoring tick flags for now
setCC cc _tick True = do
    ccI@(TxtI _ccLbl) <- costCentreLbl cc
    dflags <- use gsDynFlags
    addDependency $ OtherSymb (cc_mod cc)
                              (moduleGlobalSymbol dflags $ cc_mod cc)
    return $ jCurrentCCS |= app "h$pushCostCentre" [jCurrentCCS, e ccI]
setCC _cc _tick _push = return mempty

pushRestoreCCS :: JStat
pushRestoreCCS = appS "h$pushRestoreCCS" []

--------------------------------------------------------------------------------
-- Some cost-centre stacks to be used in generator

jCurrentCCS :: JExpr
jCurrentCCS = var "h$currentThread" .^ "ccs"

jCafCCS :: JExpr
jCafCCS = var "h$CAF"

jSystemCCS :: JExpr
jSystemCCS = var "h$CCS_SYSTEM"

--------------------------------------------------------------------------------
-- Helpers for generating profiling related things

ifProfiling :: Monoid m => m -> G m
ifProfiling m = do
    prof <- profiling
    return $ if prof then m else mempty

ifProfilingM :: Monoid m => G m -> G m
ifProfilingM m = do
    prof <- profiling
    if prof then m else return mempty

ifProfiling' :: G () -> G ()
ifProfiling' a = do
    prof <- profiling
    if prof then a else return ()

profiling :: G Bool
profiling = buildingProf <$> use gsDynFlags

profStat :: CgSettings -> JStat -> JStat
profStat s e = if csProf s then e else mempty

--------------------------------------------------------------------------------
-- Generating cost-centre and cost-centre stack variables

costCentreLbl' :: CostCentre -> G String
costCentreLbl' cc = do
    df      <- use gsDynFlags
    curModl <- use gsModule
    let lbl = show $ runSDoc (ppr cc) (initSDocContext df $ mkCodeStyle CStyle)
    return . ("h$"++) . zEncodeString $
      moduleNameColons (moduleName curModl) ++ "_" ++ if isCafCC cc then "CAF_ccs" else lbl

costCentreLbl :: CostCentre -> G Ident
costCentreLbl cc = TxtI . T.pack <$> costCentreLbl' cc

costCentreStackLbl' :: CostCentreStack -> G (Maybe String)
costCentreStackLbl' ccs = do
  df <- use gsDynFlags
  if buildingProf df then f else pure Nothing
  where
    f | isCurrentCCS ccs   = return $ Just "h$currentThread.ccs" -- FIXME
      | dontCareCCS == ccs = return $ Just "h$CCS_DONT_CARE"
      | otherwise          =
          case maybeSingletonCCS ccs of
            Just cc -> Just <$> singletonCCSLbl' cc
            Nothing -> pure Nothing

costCentreStackLbl :: CostCentreStack -> G (Maybe Ident)
costCentreStackLbl ccs = fmap (TxtI . T.pack) <$> costCentreStackLbl' ccs

singletonCCSLbl' :: CostCentre -> G String
singletonCCSLbl' cc = do
    curModl <- use gsModule
    ccLbl   <- costCentreLbl' cc
    let ccsLbl = ccLbl ++ "_ccs"
    return . zEncodeString $ moduleNameColons (moduleName curModl) <> "_" <> ccsLbl

singletonCCSLbl :: CostCentre -> G Ident
singletonCCSLbl cc = TxtI . T.pack <$> singletonCCSLbl' cc

ccsVarJ :: CostCentreStack -> G (Maybe JExpr)
ccsVarJ ccs = do
  df <- use gsDynFlags
  if (buildingProf df)
    then fmap (ValExpr . JVar) <$> costCentreStackLbl ccs
    else pure Nothing

module VectBuiltIn (
  Builtins(..),
  initBuiltins, initBuiltinTyCons, initBuiltinPAs, initBuiltinPRs
) where

#include "HsVersions.h"

import DsMonad
import IfaceEnv        ( lookupOrig )

import Module          ( Module )
import DataCon         ( DataCon )
import TyCon           ( TyCon, tyConName, tyConDataCons )
import Var             ( Var )
import Id              ( mkSysLocal )
import Name            ( Name )
import OccName         ( mkVarOccFS )

import TypeRep         ( funTyCon )
import TysPrim         ( intPrimTy )
import TysWiredIn      ( unitTyCon, tupleTyCon, intTyConName )
import PrelNames
import BasicTypes      ( Boxity(..) )

import FastString

import Control.Monad   ( liftM, zipWithM )

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , paDataCon        :: DataCon
                , preprTyCon       :: TyCon
                , prTyCon          :: TyCon
                , prDataCon        :: DataCon
                , embedTyCon       :: TyCon
                , embedDataCon     :: DataCon
                , crossTyCon       :: TyCon
                , crossDataCon     :: DataCon
                , plusTyCon        :: TyCon
                , leftDataCon      :: DataCon
                , rightDataCon     :: DataCon
                , closureTyCon     :: TyCon
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                , emptyPAVar       :: Var
                -- , packPAVar        :: Var
                -- , combinePAVar     :: Var
                , intEqPAVar       :: Var
                , liftingContext   :: Var
                }

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- dsLookupTyCon parrayTyConName
      paTyCon      <- dsLookupTyCon paTyConName
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- dsLookupTyCon preprTyConName
      prTyCon      <- dsLookupTyCon prTyConName
      let [prDataCon] = tyConDataCons prTyCon
      embedTyCon   <- dsLookupTyCon embedTyConName
      let [embedDataCon] = tyConDataCons embedTyCon
      crossTyCon   <- dsLookupTyCon ndpCrossTyConName
      let [crossDataCon] = tyConDataCons crossTyCon
      plusTyCon    <- dsLookupTyCon ndpPlusTyConName
      let [leftDataCon, rightDataCon] = tyConDataCons plusTyCon
      closureTyCon <- dsLookupTyCon closureTyConName

      mkClosureVar     <- dsLookupGlobalId mkClosureName
      applyClosureVar  <- dsLookupGlobalId applyClosureName
      mkClosurePVar    <- dsLookupGlobalId mkClosurePName
      applyClosurePVar <- dsLookupGlobalId applyClosurePName
      lengthPAVar      <- dsLookupGlobalId lengthPAName
      replicatePAVar   <- dsLookupGlobalId replicatePAName
      emptyPAVar       <- dsLookupGlobalId emptyPAName
      -- packPAVar        <- dsLookupGlobalId packPAName
      -- combinePAVar     <- dsLookupGlobalId combinePAName
      intEqPAVar       <- dsLookupGlobalId intEqPAName

      liftingContext <- liftM (\u -> mkSysLocal FSLIT("lc") u intPrimTy)
                              newUnique

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , preprTyCon       = preprTyCon
               , prTyCon          = prTyCon
               , prDataCon        = prDataCon
               , embedTyCon       = embedTyCon
               , embedDataCon     = embedDataCon
               , crossTyCon       = crossTyCon
               , crossDataCon     = crossDataCon
               , plusTyCon        = plusTyCon
               , leftDataCon      = leftDataCon
               , rightDataCon     = rightDataCon
               , closureTyCon     = closureTyCon
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               , emptyPAVar       = emptyPAVar
               -- , packPAVar        = packPAVar
               -- , combinePAVar     = combinePAVar
               , intEqPAVar       = intEqPAVar
               , liftingContext   = liftingContext
               }

initBuiltinTyCons :: DsM [(Name, TyCon)]
initBuiltinTyCons
  = do
      vects <- sequence vs
      return (zip origs vects)
  where
    (origs, vs) = unzip builtinTyCons

builtinTyCons :: [(Name, DsM TyCon)]
builtinTyCons = [(tyConName funTyCon, dsLookupTyCon closureTyConName)]

initBuiltinDicts :: [(Name, Module, FastString)] -> DsM [(Name, Var)]
initBuiltinDicts ps
  = do
      dicts <- zipWithM lookupExternalVar mods fss
      return $ zip tcs dicts
  where
    (tcs, mods, fss) = unzip3 ps

initBuiltinPAs = initBuiltinDicts builtinPAs

builtinPAs :: [(Name, Module, FastString)]
builtinPAs = [
               mk closureTyConName  nDP_CLOSURE   FSLIT("dPA_Clo")
             , mk unitTyConName     nDP_PARRAY    FSLIT("dPA_Unit")

             , mk intTyConName      nDP_INSTANCES FSLIT("dPA_Int")
             ]
             ++ tups
  where
    mk name mod fs = (name, mod, fs)

    tups = map mk_tup [2..3]
    mk_tup n = mk (tyConName $ tupleTyCon Boxed n)
                  nDP_INSTANCES
                  (mkFastString $ "dPA_" ++ show n)

initBuiltinPRs = initBuiltinDicts builtinPRs

builtinPRs :: [(Name, Module, FastString)]
builtinPRs = [
               mk (tyConName unitTyCon) nDP_PARRAY    FSLIT("dPR_Unit")
             , mk ndpCrossTyConName     nDP_PARRAY    FSLIT("dPR_Cross")
             , mk ndpPlusTyConName      nDP_PARRAY    FSLIT("dPR_Plus")
             , mk embedTyConName        nDP_PARRAY    FSLIT("dPR_Embed")
             , mk closureTyConName      nDP_CLOSURE   FSLIT("dPR_Clo")

               -- temporary
             , mk intTyConName          nDP_INSTANCES FSLIT("dPR_Int")
             ]
  where
    mk name mod fs = (name, mod, fs)

lookupExternalVar :: Module -> FastString -> DsM Var
lookupExternalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)

unitTyConName = tyConName unitTyCon


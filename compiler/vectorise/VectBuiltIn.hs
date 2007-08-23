module VectBuiltIn (
  Builtins(..),
  initBuiltins, initBuiltinTyCons, initBuiltinPAs
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

initBuiltinPAs :: DsM [(Name, Var)]
initBuiltinPAs
  = do
      pas <- zipWithM lookupExternalVar mods fss
      return $ zip tcs pas
  where
    (tcs, mods, fss) = unzip3 builtinPAs

builtinPAs :: [(Name, Module, FastString)]
builtinPAs = [
               mk closureTyConName      nDP_CLOSURE FSLIT("dPA_Clo")
             , mk (tyConName unitTyCon) nDP_PARRAY  FSLIT("dPA_Unit")

             , temporary intTyConName FSLIT("dPA_Int")
             ]
             ++ tups
  where
    mk name mod fs = (name, mod, fs)

    temporary name fs = (name, nDP_INSTANCES, fs)

    tups = map mk_tup [2..3]
    mk_tup n = temporary (tyConName $ tupleTyCon Boxed n)
                         (mkFastString $ "dPA_" ++ show n)

lookupExternalVar :: Module -> FastString -> DsM Var
lookupExternalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)


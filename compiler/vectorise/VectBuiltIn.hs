module VectBuiltIn (
  Builtins(..), initBuiltins
) where

#include "HsVersions.h"

import DsMonad

import DataCon         ( DataCon )
import TyCon           ( TyCon, tyConDataCons )
import Var             ( Var )
import Id              ( mkSysLocal )

import TysPrim         ( intPrimTy )
import PrelNames

import Control.Monad   ( liftM )

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



module VectBuiltIn (
  Builtins(..), sumTyCon, prodTyCon,
  initBuiltins, initBuiltinTyCons, initBuiltinPAs, initBuiltinPRs,

  primMethod, primPArray
) where

#include "HsVersions.h"

import DsMonad
import IfaceEnv        ( lookupOrig )

import Module          ( Module )
import DataCon         ( DataCon )
import TyCon           ( TyCon, tyConName, tyConDataCons )
import Var             ( Var )
import Id              ( mkSysLocal )
import Name            ( Name, getOccString )
import NameEnv
import OccName

import TypeRep         ( funTyCon )
import Type            ( Type )
import TysPrim
import TysWiredIn      ( unitTyCon, tupleTyCon, intTyConName )
import PrelNames
import BasicTypes      ( Boxity(..) )

import FastString
import Outputable

import Data.Array
import Control.Monad   ( liftM, zipWithM )

mAX_NDP_PROD :: Int
mAX_NDP_PROD = 3

mAX_NDP_SUM :: Int
mAX_NDP_SUM = 3

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , paDataCon        :: DataCon
                , preprTyCon       :: TyCon
                , prTyCon          :: TyCon
                , prDataCon        :: DataCon
                , parrayIntPrimTyCon :: TyCon
                , sumTyCons        :: Array Int TyCon
                , closureTyCon     :: TyCon
                , mkPRVar          :: Var
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                , emptyPAVar       :: Var
                -- , packPAVar        :: Var
                -- , combinePAVar     :: Var
                , liftingContext   :: Var
                }

sumTyCon :: Int -> Builtins -> TyCon
sumTyCon n bi
  | n >= 2 && n <= mAX_NDP_SUM = sumTyCons bi ! n
  | otherwise = pprPanic "sumTyCon" (ppr n)

prodTyCon :: Int -> Builtins -> TyCon
prodTyCon n bi
  | n >= 2 && n <= mAX_NDP_PROD = tupleTyCon Boxed n
  | otherwise = pprPanic "prodTyCon" (ppr n)

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- dsLookupTyCon parrayTyConName
      paTyCon      <- dsLookupTyCon paTyConName
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- dsLookupTyCon preprTyConName
      prTyCon      <- dsLookupTyCon prTyConName
      let [prDataCon] = tyConDataCons prTyCon
      parrayIntPrimTyCon <- dsLookupTyCon parrayIntPrimTyConName
      closureTyCon <- dsLookupTyCon closureTyConName

      sum_tcs <- mapM (lookupExternalTyCon nDP_REPR)
                      [mkFastString ("Sum" ++ show i) | i <- [2..mAX_NDP_SUM]]

      let sumTyCons = listArray (2, mAX_NDP_SUM) sum_tcs

      mkPRVar          <- dsLookupGlobalId mkPRName
      mkClosureVar     <- dsLookupGlobalId mkClosureName
      applyClosureVar  <- dsLookupGlobalId applyClosureName
      mkClosurePVar    <- dsLookupGlobalId mkClosurePName
      applyClosurePVar <- dsLookupGlobalId applyClosurePName
      lengthPAVar      <- dsLookupGlobalId lengthPAName
      replicatePAVar   <- dsLookupGlobalId replicatePAName
      emptyPAVar       <- dsLookupGlobalId emptyPAName
      -- packPAVar        <- dsLookupGlobalId packPAName
      -- combinePAVar     <- dsLookupGlobalId combinePAName

      liftingContext <- liftM (\u -> mkSysLocal FSLIT("lc") u intPrimTy)
                              newUnique

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , preprTyCon       = preprTyCon
               , prTyCon          = prTyCon
               , prDataCon        = prDataCon
               , parrayIntPrimTyCon = parrayIntPrimTyCon
               , sumTyCons        = sumTyCons
               , closureTyCon     = closureTyCon
               , mkPRVar          = mkPRVar
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               , emptyPAVar       = emptyPAVar
               -- , packPAVar        = packPAVar
               -- , combinePAVar     = combinePAVar
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
             , mk unitTyConName     nDP_INSTANCES FSLIT("dPA_Unit")

             , mk intTyConName      nDP_INSTANCES FSLIT("dPA_Int")
             ]
             ++ tups
  where
    mk name mod fs = (name, mod, fs)

    tups = map mk_tup [2..3]
    mk_tup n = mk (tyConName $ tupleTyCon Boxed n)
                  nDP_INSTANCES
                  (mkFastString $ "dPA_" ++ show n)

initBuiltinPRs = initBuiltinDicts . builtinPRs

builtinPRs :: Builtins -> [(Name, Module, FastString)]
builtinPRs bi =
  [
    mk (tyConName unitTyCon) nDP_REPR      FSLIT("dPR_Unit")
  , mk closureTyConName      nDP_CLOSURE   FSLIT("dPR_Clo")

    -- temporary
  , mk intTyConName          nDP_INSTANCES FSLIT("dPR_Int")
  ]

  ++ map mk_sum  [2..mAX_NDP_SUM]
  ++ map mk_prod [2..mAX_NDP_PROD]
  where
    mk name mod fs = (name, mod, fs)

    mk_sum n = (tyConName $ sumTyCon n bi, nDP_REPR,
                mkFastString ("dPR_Sum" ++ show n))

    mk_prod n = (tyConName $ prodTyCon n bi, nDP_REPR,
                 mkFastString ("dPR_" ++ show n))

lookupExternalVar :: Module -> FastString -> DsM Var
lookupExternalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)

lookupExternalTyCon :: Module -> FastString -> DsM TyCon
lookupExternalTyCon mod fs
  = dsLookupTyCon =<< lookupOrig mod (mkOccNameFS tcName fs)

unitTyConName = tyConName unitTyCon


primMethod :: TyCon -> String -> DsM (Maybe Var)
primMethod tycon method
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupGlobalId =<< lookupOrig nDP_PRIM (mkVarOcc $ method ++ suffix)

  | otherwise = return Nothing

primPArray :: TyCon -> DsM (Maybe TyCon)
primPArray tycon
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupTyCon =<< lookupOrig nDP_PRIM (mkOccName tcName $ "PArray" ++ suffix)

  | otherwise = return Nothing

prim_ty_cons = mkNameEnv [mk_prim intPrimTyCon]
  where
    mk_prim tycon = (tyConName tycon, '_' : getOccString tycon)

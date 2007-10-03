{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

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
import Module
import BasicTypes      ( Boxity(..) )

import FastString
import Outputable

import Data.Array
import Control.Monad   ( liftM, zipWithM )

mAX_NDP_PROD :: Int
mAX_NDP_PROD = 3

mAX_NDP_SUM :: Int
mAX_NDP_SUM = 3

mkNDPModule :: FastString -> Module
mkNDPModule m = mkModule ndpPackageId (mkModuleNameFS m)

nDP_PARRAY      = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.PArray")
nDP_REPR        = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Repr")
nDP_CLOSURE     = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Closure")
nDP_PRIM        = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Prim")
nDP_INSTANCES   = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Instances")

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , paDataCon        :: DataCon
                , preprTyCon       :: TyCon
                , prTyCon          :: TyCon
                , prDataCon        :: DataCon
                , parrayIntPrimTyCon :: TyCon
                , voidTyCon        :: TyCon
                , wrapTyCon        :: TyCon
                , sumTyCons        :: Array Int TyCon
                , closureTyCon     :: TyCon
                , voidVar          :: Var
                , mkPRVar          :: Var
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , replicatePAIntPrimVar :: Var
                , upToPAIntPrimVar :: Var
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
  | n == 1                      = wrapTyCon bi
  | n >= 0 && n <= mAX_NDP_PROD = tupleTyCon Boxed n
  | otherwise = pprPanic "prodTyCon" (ppr n)

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- externalTyCon nDP_PARRAY FSLIT("PArray")
      paTyCon      <- externalTyCon nDP_PARRAY FSLIT("PA")
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- externalTyCon nDP_PARRAY FSLIT("PRepr")
      prTyCon      <- externalTyCon nDP_PARRAY FSLIT("PR")
      let [prDataCon] = tyConDataCons prTyCon
      parrayIntPrimTyCon <- externalTyCon nDP_PRIM FSLIT("PArray_Int#")
      closureTyCon <- externalTyCon nDP_CLOSURE FSLIT(":->")

      voidTyCon    <- externalTyCon nDP_REPR FSLIT("Void")
      wrapTyCon    <- externalTyCon nDP_REPR FSLIT("Wrap")
      sum_tcs <- mapM (externalTyCon nDP_REPR)
                      [mkFastString ("Sum" ++ show i) | i <- [2..mAX_NDP_SUM]]

      let sumTyCons = listArray (2, mAX_NDP_SUM) sum_tcs

      voidVar          <- externalVar nDP_REPR FSLIT("void")
      mkPRVar          <- externalVar nDP_PARRAY FSLIT("mkPR")
      mkClosureVar     <- externalVar nDP_CLOSURE FSLIT("mkClosure")
      applyClosureVar  <- externalVar nDP_CLOSURE FSLIT("$:")
      mkClosurePVar    <- externalVar nDP_CLOSURE FSLIT("mkClosureP")
      applyClosurePVar <- externalVar nDP_CLOSURE FSLIT("$:^")
      replicatePAIntPrimVar <- externalVar nDP_PRIM FSLIT("replicatePA_Int#")
      upToPAIntPrimVar <- externalVar nDP_PRIM FSLIT("upToPA_Int#")
      lengthPAVar      <- externalVar nDP_PARRAY FSLIT("lengthPA")
      replicatePAVar   <- externalVar nDP_PARRAY FSLIT("replicatePA")
      emptyPAVar       <- externalVar nDP_PARRAY FSLIT("emptyPA")
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
               , voidTyCon        = voidTyCon
               , wrapTyCon        = wrapTyCon
               , sumTyCons        = sumTyCons
               , closureTyCon     = closureTyCon
               , voidVar          = voidVar
               , mkPRVar          = mkPRVar
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , replicatePAIntPrimVar = replicatePAIntPrimVar
               , upToPAIntPrimVar = upToPAIntPrimVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               , emptyPAVar       = emptyPAVar
               -- , packPAVar        = packPAVar
               -- , combinePAVar     = combinePAVar
               , liftingContext   = liftingContext
               }

initBuiltinTyCons :: Builtins -> [(Name, TyCon)]
initBuiltinTyCons bi = [(tyConName funTyCon, closureTyCon bi)]

initBuiltinDicts :: [(Name, Module, FastString)] -> DsM [(Name, Var)]
initBuiltinDicts ps
  = do
      dicts <- zipWithM externalVar mods fss
      return $ zip tcs dicts
  where
    (tcs, mods, fss) = unzip3 ps

initBuiltinPAs = initBuiltinDicts . builtinPAs

builtinPAs :: Builtins -> [(Name, Module, FastString)]
builtinPAs bi
  = [
      mk (tyConName $ closureTyCon bi)  nDP_CLOSURE     FSLIT("dPA_Clo")
    , mk (tyConName $ voidTyCon bi)     nDP_REPR        FSLIT("dPA_Void")
    , mk unitTyConName                  nDP_INSTANCES   FSLIT("dPA_Unit")

    , mk intTyConName                   nDP_INSTANCES   FSLIT("dPA_Int")
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
    mk (tyConName unitTyCon)          nDP_REPR      FSLIT("dPR_Unit")
  , mk (tyConName $ voidTyCon bi)     nDP_REPR      FSLIT("dPR_Void")
  , mk (tyConName $ wrapTyCon bi)     nDP_REPR      FSLIT("dPR_Wrap")
  , mk (tyConName $ closureTyCon bi)  nDP_CLOSURE   FSLIT("dPR_Clo")

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

externalVar :: Module -> FastString -> DsM Var
externalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)

externalTyCon :: Module -> FastString -> DsM TyCon
externalTyCon mod fs
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

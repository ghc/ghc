module VectBuiltIn (
  Builtins(..), sumTyCon, prodTyCon, uarrTy, intPrimArrayTy,
  combinePAVar,
  initBuiltins, initBuiltinVars, initBuiltinTyCons, initBuiltinDataCons,
  initBuiltinPAs, initBuiltinPRs,
  initBuiltinBoxedTyCons,

  primMethod, primPArray
) where

#include "HsVersions.h"

import DsMonad
import IfaceEnv        ( lookupOrig )

import Module
import DataCon         ( DataCon, dataConName, dataConWorkId )
import TyCon           ( TyCon, tyConName, tyConDataCons )
import Var             ( Var )
import Id              ( mkSysLocal )
import Name            ( Name, getOccString )
import NameEnv
import OccName

import TypeRep         ( funTyCon )
import Type            ( Type, mkTyConApp )
import TysPrim
import TysWiredIn      ( unitTyCon, unitDataCon,
                         tupleTyCon,
                         intTyCon, intTyConName, intTy,
                         doubleTyCon, doubleTyConName,
                         boolTyCon, boolTyConName, trueDataCon, falseDataCon,
                         parrTyConName )
import PrelNames       ( gHC_PARR )
import BasicTypes      ( Boxity(..) )

import FastString
import Outputable

import Data.Array
import Control.Monad   ( liftM, zipWithM )
import Data.List       ( unzip4 )

mAX_NDP_PROD :: Int
mAX_NDP_PROD = 5

mAX_NDP_SUM :: Int
mAX_NDP_SUM = 3

mAX_NDP_COMBINE :: Int
mAX_NDP_COMBINE = 2

mkNDPModule :: FastString -> Module
mkNDPModule m = mkModule ndpPackageId (mkModuleNameFS m)

nDP_UARR, nDP_PARRAY, nDP_REPR, nDP_CLOSURE, nDP_UNBOXED, nDP_INSTANCES, nDP_COMBINATORS,
    nDP_PRELUDE_PARR, nDP_PRELUDE_INT, nDP_PRELUDE_DOUBLE :: Module

nDP_UARR        = mkNDPModule FSLIT("Data.Array.Parallel.Unlifted.Flat.UArr")
nDP_PARRAY      = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.PArray")
nDP_REPR        = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Repr")
nDP_CLOSURE     = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Closure")
nDP_UNBOXED     = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Unboxed")
nDP_INSTANCES   = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Instances")
nDP_COMBINATORS = mkNDPModule FSLIT("Data.Array.Parallel.Lifted.Combinators")

nDP_PRELUDE_PARR = mkNDPModule FSLIT("Data.Array.Parallel.Prelude.Base.PArr")
nDP_PRELUDE_INT  = mkNDPModule FSLIT("Data.Array.Parallel.Prelude.Base.Int")
nDP_PRELUDE_DOUBLE = mkNDPModule FSLIT("Data.Array.Parallel.Prelude.Base.Double")

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , paDataCon        :: DataCon
                , preprTyCon       :: TyCon
                , prTyCon          :: TyCon
                , prDataCon        :: DataCon
                , uarrTyCon        :: TyCon
                , voidTyCon        :: TyCon
                , wrapTyCon        :: TyCon
                , enumerationTyCon :: TyCon
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
                , selectPAIntPrimVar :: Var
                , truesPABoolPrimVar :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                , emptyPAVar       :: Var
                , packPAVar        :: Var
                , combinePAVars    :: Array Int Var
                , liftingContext   :: Var
                }

uarrTy :: Type -> Builtins -> Type
uarrTy ty bi = mkTyConApp (uarrTyCon bi) [ty]

intPrimArrayTy :: Builtins -> Type
intPrimArrayTy = uarrTy intTy

sumTyCon :: Int -> Builtins -> TyCon
sumTyCon n bi
  | n >= 2 && n <= mAX_NDP_SUM = sumTyCons bi ! n
  | otherwise = pprPanic "sumTyCon" (ppr n)

prodTyCon :: Int -> Builtins -> TyCon
prodTyCon n bi
  | n == 1                      = wrapTyCon bi
  | n >= 0 && n <= mAX_NDP_PROD = tupleTyCon Boxed n
  | otherwise = pprPanic "prodTyCon" (ppr n)

combinePAVar :: Int -> Builtins -> Var
combinePAVar n bi
  | n >= 2 && n <= mAX_NDP_COMBINE = combinePAVars bi ! n
  | otherwise = pprPanic "combinePAVar" (ppr n)

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- externalTyCon nDP_PARRAY FSLIT("PArray")
      paTyCon      <- externalTyCon nDP_PARRAY FSLIT("PA")
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- externalTyCon nDP_PARRAY FSLIT("PRepr")
      prTyCon      <- externalTyCon nDP_PARRAY FSLIT("PR")
      let [prDataCon] = tyConDataCons prTyCon
      uarrTyCon    <- externalTyCon nDP_UARR   FSLIT("UArr")
      closureTyCon <- externalTyCon nDP_CLOSURE FSLIT(":->")

      voidTyCon    <- externalTyCon nDP_REPR FSLIT("Void")
      wrapTyCon    <- externalTyCon nDP_REPR FSLIT("Wrap")
      enumerationTyCon <- externalTyCon nDP_REPR FSLIT("Enumeration")
      sum_tcs <- mapM (externalTyCon nDP_REPR)
                      [mkFastString ("Sum" ++ show i) | i <- [2..mAX_NDP_SUM]]

      let sumTyCons = listArray (2, mAX_NDP_SUM) sum_tcs

      voidVar          <- externalVar nDP_REPR FSLIT("void")
      mkPRVar          <- externalVar nDP_PARRAY FSLIT("mkPR")
      mkClosureVar     <- externalVar nDP_CLOSURE FSLIT("mkClosure")
      applyClosureVar  <- externalVar nDP_CLOSURE FSLIT("$:")
      mkClosurePVar    <- externalVar nDP_CLOSURE FSLIT("mkClosureP")
      applyClosurePVar <- externalVar nDP_CLOSURE FSLIT("$:^")
      replicatePAIntPrimVar <- externalVar nDP_UNBOXED FSLIT("replicatePA_Int#")
      upToPAIntPrimVar <- externalVar nDP_UNBOXED FSLIT("upToPA_Int#")
      selectPAIntPrimVar <- externalVar nDP_UNBOXED FSLIT("selectPA_Int#")
      truesPABoolPrimVar <- externalVar nDP_UNBOXED FSLIT("truesPA_Bool#")
      lengthPAVar      <- externalVar nDP_PARRAY FSLIT("lengthPA#")
      replicatePAVar   <- externalVar nDP_PARRAY FSLIT("replicatePA#")
      emptyPAVar       <- externalVar nDP_PARRAY FSLIT("emptyPA")
      packPAVar        <- externalVar nDP_PARRAY FSLIT("packPA#")

      combines <- mapM (externalVar nDP_PARRAY)
                       [mkFastString ("combine" ++ show i ++ "PA#")
                          | i <- [2..mAX_NDP_COMBINE]]
      let combinePAVars = listArray (2, mAX_NDP_COMBINE) combines

      liftingContext <- liftM (\u -> mkSysLocal FSLIT("lc") u intPrimTy)
                              newUnique

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , preprTyCon       = preprTyCon
               , prTyCon          = prTyCon
               , prDataCon        = prDataCon
               , uarrTyCon        = uarrTyCon
               , voidTyCon        = voidTyCon
               , wrapTyCon        = wrapTyCon
               , enumerationTyCon = enumerationTyCon
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
               , selectPAIntPrimVar = selectPAIntPrimVar
               , truesPABoolPrimVar = truesPABoolPrimVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               , emptyPAVar       = emptyPAVar
               , packPAVar        = packPAVar
               , combinePAVars    = combinePAVars
               , liftingContext   = liftingContext
               }

initBuiltinVars :: Builtins -> DsM [(Var, Var)]
initBuiltinVars _
  = do
      uvars <- zipWithM externalVar umods ufs
      vvars <- zipWithM externalVar vmods vfs
      return $ [(v,v) | v <- map dataConWorkId defaultDataConWorkers]
               ++ zip uvars vvars
  where
    (umods, ufs, vmods, vfs) = unzip4 preludeVars

defaultDataConWorkers :: [DataCon]
defaultDataConWorkers = [trueDataCon, falseDataCon, unitDataCon]

preludeVars :: [(Module, FastString, Module, FastString)]
preludeVars
  = [
      mk gHC_PARR FSLIT("mapP")       nDP_COMBINATORS FSLIT("mapPA")
    , mk gHC_PARR FSLIT("zipWithP")   nDP_COMBINATORS FSLIT("zipWithPA")
    , mk gHC_PARR FSLIT("zipP")       nDP_COMBINATORS FSLIT("zipPA")
    , mk gHC_PARR FSLIT("unzipP")     nDP_COMBINATORS FSLIT("unzipPA")
    , mk gHC_PARR FSLIT("filterP")    nDP_COMBINATORS FSLIT("filterPA")
    , mk gHC_PARR FSLIT("lengthP")    nDP_COMBINATORS FSLIT("lengthPA")
    , mk gHC_PARR FSLIT("replicateP") nDP_COMBINATORS FSLIT("replicatePA")
    , mk gHC_PARR FSLIT("!:")         nDP_COMBINATORS FSLIT("indexPA")
    , mk gHC_PARR FSLIT("crossMapP")  nDP_COMBINATORS FSLIT("crossMapPA")
    , mk gHC_PARR FSLIT("singletonP") nDP_COMBINATORS FSLIT("singletonPA")
    , mk gHC_PARR FSLIT("concatP")    nDP_COMBINATORS FSLIT("concatPA")
    , mk gHC_PARR FSLIT("+:+")        nDP_COMBINATORS FSLIT("appPA")

    , mk nDP_PRELUDE_INT  FSLIT("plus") nDP_PRELUDE_INT FSLIT("plusV")
    , mk nDP_PRELUDE_INT  FSLIT("minus") nDP_PRELUDE_INT FSLIT("minusV")
    , mk nDP_PRELUDE_INT  FSLIT("mult")  nDP_PRELUDE_INT FSLIT("multV")
    , mk nDP_PRELUDE_INT  FSLIT("intDiv")  nDP_PRELUDE_INT FSLIT("intDivV")
    , mk nDP_PRELUDE_INT  FSLIT("sumP")  nDP_PRELUDE_INT FSLIT("sumPA")
    , mk nDP_PRELUDE_INT  FSLIT("upToP") nDP_PRELUDE_INT FSLIT("upToPA")

    , mk nDP_PRELUDE_INT  FSLIT("eq") nDP_PRELUDE_INT FSLIT("eqV")
    , mk nDP_PRELUDE_INT  FSLIT("neq") nDP_PRELUDE_INT FSLIT("neqV")
    , mk nDP_PRELUDE_INT  FSLIT("le")  nDP_PRELUDE_INT FSLIT("leV")
    , mk nDP_PRELUDE_INT  FSLIT("lt") nDP_PRELUDE_INT FSLIT("ltV")
    , mk nDP_PRELUDE_INT  FSLIT("ge") nDP_PRELUDE_INT FSLIT("geV")
    , mk nDP_PRELUDE_INT  FSLIT("gt")  nDP_PRELUDE_INT FSLIT("gtV")

    , mk nDP_PRELUDE_DOUBLE  FSLIT("plus") nDP_PRELUDE_DOUBLE FSLIT("plusV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("minus") nDP_PRELUDE_DOUBLE FSLIT("minusV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("mult")  nDP_PRELUDE_DOUBLE FSLIT("multV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("divide")  nDP_PRELUDE_DOUBLE FSLIT("divideV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("sumP")  nDP_PRELUDE_DOUBLE FSLIT("sumPA")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("minIndexP") 
         nDP_PRELUDE_DOUBLE  FSLIT("minIndexPA")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("maxIndexP")
         nDP_PRELUDE_DOUBLE  FSLIT("maxIndexPA")

    , mk nDP_PRELUDE_DOUBLE  FSLIT("eq") nDP_PRELUDE_DOUBLE FSLIT("eqV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("neq") nDP_PRELUDE_DOUBLE FSLIT("neqV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("le")  nDP_PRELUDE_DOUBLE FSLIT("leV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("lt") nDP_PRELUDE_DOUBLE FSLIT("ltV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("ge") nDP_PRELUDE_DOUBLE FSLIT("geV")
    , mk nDP_PRELUDE_DOUBLE  FSLIT("gt")  nDP_PRELUDE_DOUBLE FSLIT("gtV")

    -- FIXME: temporary
    , mk nDP_PRELUDE_PARR FSLIT("fromPArrayP") nDP_PRELUDE_PARR FSLIT("fromPArrayPA")
    , mk nDP_PRELUDE_PARR FSLIT("toPArrayP") nDP_PRELUDE_PARR FSLIT("toPArrayPA")
    , mk nDP_PRELUDE_PARR FSLIT("fromNestedPArrayP") nDP_PRELUDE_PARR FSLIT("fromNestedPArrayPA")
    ]
  where
    mk = (,,,)

initBuiltinTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinTyCons bi
  = do
      -- parr <- externalTyCon nDP_PRELUDE_PARR FSLIT("PArr")
      return $ (tyConName funTyCon, closureTyCon bi)
             : (parrTyConName,      parrayTyCon bi)

             -- FIXME: temporary
             : (tyConName $ parrayTyCon bi, parrayTyCon bi)

             : [(tyConName tc, tc) | tc <- defaultTyCons]

defaultTyCons :: [TyCon]
defaultTyCons = [intTyCon, boolTyCon, doubleTyCon]

initBuiltinDataCons :: Builtins -> [(Name, DataCon)]
initBuiltinDataCons _ = [(dataConName dc, dc)| dc <- defaultDataCons]

defaultDataCons :: [DataCon]
defaultDataCons = [trueDataCon, falseDataCon, unitDataCon]

initBuiltinDicts :: [(Name, Module, FastString)] -> DsM [(Name, Var)]
initBuiltinDicts ps
  = do
      dicts <- zipWithM externalVar mods fss
      return $ zip tcs dicts
  where
    (tcs, mods, fss) = unzip3 ps

initBuiltinPAs :: Builtins -> DsM [(Name, Var)]
initBuiltinPAs = initBuiltinDicts . builtinPAs

builtinPAs :: Builtins -> [(Name, Module, FastString)]
builtinPAs bi
  = [
      mk (tyConName $ closureTyCon bi)  nDP_CLOSURE     FSLIT("dPA_Clo")
    , mk (tyConName $ voidTyCon bi)     nDP_REPR        FSLIT("dPA_Void")
    , mk (tyConName $ parrayTyCon bi)   nDP_INSTANCES   FSLIT("dPA_PArray")
    , mk unitTyConName                  nDP_INSTANCES   FSLIT("dPA_Unit")

    , mk intTyConName                   nDP_INSTANCES   FSLIT("dPA_Int")
    , mk doubleTyConName                nDP_INSTANCES   FSLIT("dPA_Double")
    , mk boolTyConName                  nDP_INSTANCES   FSLIT("dPA_Bool")
    ]
    ++ tups
  where
    mk name mod fs = (name, mod, fs)

    tups = map mk_tup [2..mAX_NDP_PROD]
    mk_tup n = mk (tyConName $ tupleTyCon Boxed n)
                  nDP_INSTANCES
                  (mkFastString $ "dPA_" ++ show n)

initBuiltinPRs :: Builtins -> DsM [(Name, Var)]
initBuiltinPRs = initBuiltinDicts . builtinPRs

builtinPRs :: Builtins -> [(Name, Module, FastString)]
builtinPRs bi =
  [
    mk (tyConName unitTyCon)          nDP_REPR      FSLIT("dPR_Unit")
  , mk (tyConName $ voidTyCon bi)     nDP_REPR      FSLIT("dPR_Void")
  , mk (tyConName $ wrapTyCon bi)     nDP_REPR      FSLIT("dPR_Wrap")
  , mk (tyConName $ enumerationTyCon bi) nDP_REPR   FSLIT("dPR_Enumeration")
  , mk (tyConName $ closureTyCon bi)  nDP_CLOSURE   FSLIT("dPR_Clo")

    -- temporary
  , mk intTyConName          nDP_INSTANCES FSLIT("dPR_Int")
  , mk doubleTyConName       nDP_INSTANCES FSLIT("dPR_Double")
  ]

  ++ map mk_sum  [2..mAX_NDP_SUM]
  ++ map mk_prod [2..mAX_NDP_PROD]
  where
    mk name mod fs = (name, mod, fs)

    mk_sum n = (tyConName $ sumTyCon n bi, nDP_REPR,
                mkFastString ("dPR_Sum" ++ show n))

    mk_prod n = (tyConName $ prodTyCon n bi, nDP_REPR,
                 mkFastString ("dPR_" ++ show n))

initBuiltinBoxedTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinBoxedTyCons = return . builtinBoxedTyCons

builtinBoxedTyCons :: Builtins -> [(Name, TyCon)]
builtinBoxedTyCons _ =
  [(tyConName intPrimTyCon, intTyCon)]

externalVar :: Module -> FastString -> DsM Var
externalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)

externalTyCon :: Module -> FastString -> DsM TyCon
externalTyCon mod fs
  = dsLookupTyCon =<< lookupOrig mod (mkOccNameFS tcName fs)

unitTyConName :: Name
unitTyConName = tyConName unitTyCon


primMethod :: TyCon -> String -> DsM (Maybe Var)
primMethod tycon method
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupGlobalId =<< lookupOrig nDP_UNBOXED (mkVarOcc $ method ++ suffix)

  | otherwise = return Nothing

primPArray :: TyCon -> DsM (Maybe TyCon)
primPArray tycon
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupTyCon =<< lookupOrig nDP_UNBOXED (mkOccName tcName $ "PArray" ++ suffix)

  | otherwise = return Nothing

prim_ty_cons :: NameEnv String
prim_ty_cons = mkNameEnv [mk_prim intPrimTyCon]
  where
    mk_prim tycon = (tyConName tycon, '_' : getOccString tycon)


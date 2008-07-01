module VectBuiltIn (
  Builtins(..), sumTyCon, prodTyCon,
  combinePAVar,
  initBuiltins, initBuiltinVars, initBuiltinTyCons, initBuiltinDataCons,
  initBuiltinPAs, initBuiltinPRs,
  initBuiltinBoxedTyCons,

  primMethod, primPArray
) where

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
                         tupleTyCon, tupleCon,
                         intTyCon, intTyConName,
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

nDP_PARRAY,
  nDP_REPR,
  nDP_CLOSURE,
  nDP_UNBOXED,
  nDP_INSTANCES,
  nDP_COMBINATORS,
  nDP_PRELUDE_PARR,
  nDP_PRELUDE_INT,
  nDP_PRELUDE_DOUBLE,
  nDP_PRELUDE_BOOL,
  nDP_PRELUDE_TUPLE :: Module

nDP_PARRAY      = mkNDPModule (fsLit "Data.Array.Parallel.Lifted.PArray")
nDP_REPR        = mkNDPModule (fsLit "Data.Array.Parallel.Lifted.Repr")
nDP_CLOSURE     = mkNDPModule (fsLit "Data.Array.Parallel.Lifted.Closure")
nDP_UNBOXED     = mkNDPModule (fsLit "Data.Array.Parallel.Lifted.Unboxed")
nDP_INSTANCES   = mkNDPModule (fsLit "Data.Array.Parallel.Lifted.Instances")
nDP_COMBINATORS = mkNDPModule (fsLit "Data.Array.Parallel.Lifted.Combinators")

nDP_PRELUDE_PARR = mkNDPModule (fsLit "Data.Array.Parallel.Prelude.Base.PArr")
nDP_PRELUDE_INT  = mkNDPModule (fsLit "Data.Array.Parallel.Prelude.Base.Int")
nDP_PRELUDE_DOUBLE = mkNDPModule (fsLit "Data.Array.Parallel.Prelude.Base.Double")
nDP_PRELUDE_BOOL = mkNDPModule (fsLit "Data.Array.Parallel.Prelude.Base.Bool")
nDP_PRELUDE_TUPLE  = mkNDPModule (fsLit "Data.Array.Parallel.Prelude.Base.Tuple")

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , paDataCon        :: DataCon
                , preprTyCon       :: TyCon
                , prTyCon          :: TyCon
                , prDataCon        :: DataCon
                , intPrimArrayTy   :: Type
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
      parrayTyCon  <- externalTyCon nDP_PARRAY (fsLit "PArray")
      paTyCon      <- externalTyCon nDP_PARRAY (fsLit "PA")
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- externalTyCon nDP_PARRAY (fsLit "PRepr")
      prTyCon      <- externalTyCon nDP_PARRAY (fsLit "PR")
      let [prDataCon] = tyConDataCons prTyCon
      intPrimArrayTy <- externalType nDP_UNBOXED (fsLit "PArray_Int#")
      closureTyCon <- externalTyCon nDP_CLOSURE (fsLit ":->")

      voidTyCon    <- externalTyCon nDP_REPR (fsLit "Void")
      wrapTyCon    <- externalTyCon nDP_REPR (fsLit "Wrap")
      enumerationTyCon <- externalTyCon nDP_REPR (fsLit "Enumeration")
      sum_tcs <- mapM (externalTyCon nDP_REPR)
                      [mkFastString ("Sum" ++ show i) | i <- [2..mAX_NDP_SUM]]

      let sumTyCons = listArray (2, mAX_NDP_SUM) sum_tcs

      voidVar          <- externalVar nDP_REPR (fsLit "void")
      mkPRVar          <- externalVar nDP_PARRAY (fsLit "mkPR")
      mkClosureVar     <- externalVar nDP_CLOSURE (fsLit "mkClosure")
      applyClosureVar  <- externalVar nDP_CLOSURE (fsLit "$:")
      mkClosurePVar    <- externalVar nDP_CLOSURE (fsLit "mkClosureP")
      applyClosurePVar <- externalVar nDP_CLOSURE (fsLit "$:^")
      replicatePAIntPrimVar <- externalVar nDP_UNBOXED (fsLit "replicatePA_Int#")
      upToPAIntPrimVar <- externalVar nDP_UNBOXED (fsLit "upToPA_Int#")
      selectPAIntPrimVar <- externalVar nDP_UNBOXED (fsLit "selectPA_Int#")
      truesPABoolPrimVar <- externalVar nDP_UNBOXED (fsLit "truesPA_Bool#")
      lengthPAVar      <- externalVar nDP_PARRAY (fsLit "lengthPA#")
      replicatePAVar   <- externalVar nDP_PARRAY (fsLit "replicatePA#")
      emptyPAVar       <- externalVar nDP_PARRAY (fsLit "emptyPA")
      packPAVar        <- externalVar nDP_PARRAY (fsLit "packPA#")

      combines <- mapM (externalVar nDP_PARRAY)
                       [mkFastString ("combine" ++ show i ++ "PA#")
                          | i <- [2..mAX_NDP_COMBINE]]
      let combinePAVars = listArray (2, mAX_NDP_COMBINE) combines

      liftingContext <- liftM (\u -> mkSysLocal (fsLit "lc") u intPrimTy)
                              newUnique

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , preprTyCon       = preprTyCon
               , prTyCon          = prTyCon
               , prDataCon        = prDataCon
               , intPrimArrayTy   = intPrimArrayTy
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
      cvars <- zipWithM externalVar cmods cfs
      return $ [(v,v) | v <- map dataConWorkId defaultDataConWorkers]
               ++ zip (map dataConWorkId cons) cvars
               ++ zip uvars vvars
  where
    (umods, ufs, vmods, vfs) = unzip4 preludeVars

    (cons, cmods, cfs) = unzip3 preludeDataCons

defaultDataConWorkers :: [DataCon]
defaultDataConWorkers = [trueDataCon, falseDataCon, unitDataCon]

preludeDataCons :: [(DataCon, Module, FastString)]
preludeDataCons
  = [mk_tup n nDP_PRELUDE_TUPLE (mkFastString $ "tup" ++ show n) | n <- [2..3]]
  where
    mk_tup n mod name = (tupleCon Boxed n, mod, name)

preludeVars :: [(Module, FastString, Module, FastString)]
preludeVars
  = [
      mk gHC_PARR (fsLit "mapP")       nDP_COMBINATORS (fsLit "mapPA")
    , mk gHC_PARR (fsLit "zipWithP")   nDP_COMBINATORS (fsLit "zipWithPA")
    , mk gHC_PARR (fsLit "zipP")       nDP_COMBINATORS (fsLit "zipPA")
    , mk gHC_PARR (fsLit "unzipP")     nDP_COMBINATORS (fsLit "unzipPA")
    , mk gHC_PARR (fsLit "filterP")    nDP_COMBINATORS (fsLit "filterPA")
    , mk gHC_PARR (fsLit "lengthP")    nDP_COMBINATORS (fsLit "lengthPA")
    , mk gHC_PARR (fsLit "replicateP") nDP_COMBINATORS (fsLit "replicatePA")
    , mk gHC_PARR (fsLit "!:")         nDP_COMBINATORS (fsLit "indexPA")
    , mk gHC_PARR (fsLit "crossMapP")  nDP_COMBINATORS (fsLit "crossMapPA")
    , mk gHC_PARR (fsLit "singletonP") nDP_COMBINATORS (fsLit "singletonPA")
    , mk gHC_PARR (fsLit "concatP")    nDP_COMBINATORS (fsLit "concatPA")
    , mk gHC_PARR (fsLit "+:+")        nDP_COMBINATORS (fsLit "appPA")
    , mk gHC_PARR (fsLit "emptyP")     nDP_PARRAY (fsLit "emptyPA")

    , mk nDP_PRELUDE_INT  (fsLit "plus") nDP_PRELUDE_INT (fsLit "plusV")
    , mk nDP_PRELUDE_INT  (fsLit "minus") nDP_PRELUDE_INT (fsLit "minusV")
    , mk nDP_PRELUDE_INT  (fsLit "mult")  nDP_PRELUDE_INT (fsLit "multV")
    , mk nDP_PRELUDE_INT  (fsLit "intDiv")  nDP_PRELUDE_INT (fsLit "intDivV")
    , mk nDP_PRELUDE_INT  (fsLit "intMod")  nDP_PRELUDE_INT (fsLit "intModV")
    , mk nDP_PRELUDE_INT  (fsLit "intSquareRoot")  nDP_PRELUDE_INT (fsLit "intSquareRootV")
    , mk nDP_PRELUDE_INT  (fsLit "intSumP")  nDP_PRELUDE_INT (fsLit "intSumPA")
    , mk nDP_PRELUDE_INT  (fsLit "enumFromToP")  nDP_PRELUDE_INT (fsLit "enumFromToPA")
    , mk nDP_PRELUDE_INT  (fsLit "upToP") nDP_PRELUDE_INT (fsLit "upToPA")

    , mk nDP_PRELUDE_INT  (fsLit "eq") nDP_PRELUDE_INT (fsLit "eqV")
    , mk nDP_PRELUDE_INT  (fsLit "neq") nDP_PRELUDE_INT (fsLit "neqV")
    , mk nDP_PRELUDE_INT  (fsLit "le")  nDP_PRELUDE_INT (fsLit "leV")
    , mk nDP_PRELUDE_INT  (fsLit "lt") nDP_PRELUDE_INT (fsLit "ltV")
    , mk nDP_PRELUDE_INT  (fsLit "ge") nDP_PRELUDE_INT (fsLit "geV")
    , mk nDP_PRELUDE_INT  (fsLit "gt")  nDP_PRELUDE_INT (fsLit "gtV")

    , mk nDP_PRELUDE_DOUBLE  (fsLit "plus") nDP_PRELUDE_DOUBLE (fsLit "plusV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "minus") nDP_PRELUDE_DOUBLE (fsLit "minusV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "mult")  nDP_PRELUDE_DOUBLE (fsLit "multV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "divide")  nDP_PRELUDE_DOUBLE (fsLit "divideV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit  "squareRoot")  nDP_PRELUDE_DOUBLE (fsLit "squareRootV")    
    , mk nDP_PRELUDE_DOUBLE  (fsLit "doubleSumP")  nDP_PRELUDE_DOUBLE (fsLit "doubleSumPA")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "minIndexP") 
         nDP_PRELUDE_DOUBLE  (fsLit "minIndexPA")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "maxIndexP")
         nDP_PRELUDE_DOUBLE  (fsLit "maxIndexPA")

    , mk nDP_PRELUDE_DOUBLE  (fsLit "eq") nDP_PRELUDE_DOUBLE (fsLit "eqV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "neq") nDP_PRELUDE_DOUBLE (fsLit "neqV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "le")  nDP_PRELUDE_DOUBLE (fsLit "leV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "lt") nDP_PRELUDE_DOUBLE (fsLit "ltV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "ge") nDP_PRELUDE_DOUBLE (fsLit "geV")
    , mk nDP_PRELUDE_DOUBLE  (fsLit "gt")  nDP_PRELUDE_DOUBLE (fsLit "gtV")

    , mk nDP_PRELUDE_BOOL  (fsLit "andP")  nDP_PRELUDE_BOOL (fsLit "andPA")
    , mk nDP_PRELUDE_BOOL  (fsLit "orP")  nDP_PRELUDE_BOOL (fsLit "orPA")

    -- FIXME: temporary
    , mk nDP_PRELUDE_PARR (fsLit "fromPArrayP") nDP_PRELUDE_PARR (fsLit "fromPArrayPA")
    , mk nDP_PRELUDE_PARR (fsLit "toPArrayP") nDP_PRELUDE_PARR (fsLit "toPArrayPA")
    , mk nDP_PRELUDE_PARR (fsLit "fromNestedPArrayP") nDP_PRELUDE_PARR (fsLit "fromNestedPArrayPA")
    , mk nDP_PRELUDE_PARR (fsLit "combineP")    nDP_COMBINATORS (fsLit "combine2PA")
    ]
  where
    mk = (,,,)

initBuiltinTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinTyCons bi
  = do
      -- parr <- externalTyCon nDP_PRELUDE_PARR (fsLit "PArr")
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
      mk (tyConName $ closureTyCon bi)  nDP_CLOSURE     (fsLit "dPA_Clo")
    , mk (tyConName $ voidTyCon bi)     nDP_REPR        (fsLit "dPA_Void")
    , mk (tyConName $ parrayTyCon bi)   nDP_INSTANCES   (fsLit "dPA_PArray")
    , mk unitTyConName                  nDP_INSTANCES   (fsLit "dPA_Unit")

    , mk intTyConName                   nDP_INSTANCES   (fsLit "dPA_Int")
    , mk doubleTyConName                nDP_INSTANCES   (fsLit "dPA_Double")
    , mk boolTyConName                  nDP_INSTANCES   (fsLit "dPA_Bool")
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
    mk (tyConName unitTyCon)          nDP_REPR      (fsLit "dPR_Unit")
  , mk (tyConName $ voidTyCon bi)     nDP_REPR      (fsLit "dPR_Void")
  , mk (tyConName $ wrapTyCon bi)     nDP_REPR      (fsLit "dPR_Wrap")
  , mk (tyConName $ enumerationTyCon bi) nDP_REPR   (fsLit "dPR_Enumeration")
  , mk (tyConName $ closureTyCon bi)  nDP_CLOSURE   (fsLit "dPR_Clo")

    -- temporary
  , mk intTyConName          nDP_INSTANCES (fsLit "dPR_Int")
  , mk doubleTyConName       nDP_INSTANCES (fsLit "dPR_Double")
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

externalType :: Module -> FastString -> DsM Type
externalType mod fs
  = do
      tycon <- externalTyCon mod fs
      return $ mkTyConApp tycon []

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


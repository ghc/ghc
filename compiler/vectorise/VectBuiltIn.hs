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

mAX_DPH_PROD :: Int
mAX_DPH_PROD = 5

mAX_DPH_SUM :: Int
mAX_DPH_SUM = 3

mAX_DPH_COMBINE :: Int
mAX_DPH_COMBINE = 2

data Modules = Modules {
                   dph_PArray :: Module
                 , dph_Repr :: Module
                 , dph_Closure :: Module
                 , dph_Unboxed :: Module
                 , dph_Instances :: Module
                 , dph_Combinators :: Module
                 , dph_Prelude_PArr :: Module
                 , dph_Prelude_Int :: Module
                 , dph_Prelude_Double :: Module
                 , dph_Prelude_Bool :: Module
                 , dph_Prelude_Tuple :: Module
               }

dph_Modules :: PackageId -> Modules
dph_Modules pkg = Modules {
    dph_PArray         = mk (fsLit "Data.Array.Parallel.Lifted.PArray")
  , dph_Repr           = mk (fsLit "Data.Array.Parallel.Lifted.Repr")
  , dph_Closure        = mk (fsLit "Data.Array.Parallel.Lifted.Closure")
  , dph_Unboxed        = mk (fsLit "Data.Array.Parallel.Lifted.Unboxed")
  , dph_Instances      = mk (fsLit "Data.Array.Parallel.Lifted.Instances")
  , dph_Combinators    = mk (fsLit "Data.Array.Parallel.Lifted.Combinators")

  , dph_Prelude_PArr   = mk (fsLit "Data.Array.Parallel.Prelude.Base.PArr")
  , dph_Prelude_Int    = mk (fsLit "Data.Array.Parallel.Prelude.Base.Int")
  , dph_Prelude_Double = mk (fsLit "Data.Array.Parallel.Prelude.Base.Double")
  , dph_Prelude_Bool   = mk (fsLit "Data.Array.Parallel.Prelude.Base.Bool")
  , dph_Prelude_Tuple  = mk (fsLit "Data.Array.Parallel.Prelude.Base.Tuple")
  }
  where
    mk = mkModule pkg . mkModuleNameFS


data Builtins = Builtins {
                  dphModules       :: Modules
                , parrayTyCon      :: TyCon
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
  | n >= 2 && n <= mAX_DPH_SUM = sumTyCons bi ! n
  | otherwise = pprPanic "sumTyCon" (ppr n)

prodTyCon :: Int -> Builtins -> TyCon
prodTyCon n bi
  | n == 1                      = wrapTyCon bi
  | n >= 0 && n <= mAX_DPH_PROD = tupleTyCon Boxed n
  | otherwise = pprPanic "prodTyCon" (ppr n)

combinePAVar :: Int -> Builtins -> Var
combinePAVar n bi
  | n >= 2 && n <= mAX_DPH_COMBINE = combinePAVars bi ! n
  | otherwise = pprPanic "combinePAVar" (ppr n)

initBuiltins :: PackageId -> DsM Builtins
initBuiltins pkg
  = do
      parrayTyCon  <- externalTyCon dph_PArray (fsLit "PArray")
      paTyCon      <- externalTyCon dph_PArray (fsLit "PA")
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- externalTyCon dph_PArray (fsLit "PRepr")
      prTyCon      <- externalTyCon dph_PArray (fsLit "PR")
      let [prDataCon] = tyConDataCons prTyCon
      intPrimArrayTy <- externalType dph_Unboxed (fsLit "PArray_Int#")
      closureTyCon <- externalTyCon dph_Closure (fsLit ":->")

      voidTyCon    <- externalTyCon dph_Repr (fsLit "Void")
      wrapTyCon    <- externalTyCon dph_Repr (fsLit "Wrap")
      enumerationTyCon <- externalTyCon dph_Repr (fsLit "Enumeration")
      sum_tcs <- mapM (externalTyCon dph_Repr)
                      [mkFastString ("Sum" ++ show i) | i <- [2..mAX_DPH_SUM]]

      let sumTyCons = listArray (2, mAX_DPH_SUM) sum_tcs

      voidVar          <- externalVar dph_Repr (fsLit "void")
      mkPRVar          <- externalVar dph_PArray (fsLit "mkPR")
      mkClosureVar     <- externalVar dph_Closure (fsLit "mkClosure")
      applyClosureVar  <- externalVar dph_Closure (fsLit "$:")
      mkClosurePVar    <- externalVar dph_Closure (fsLit "mkClosureP")
      applyClosurePVar <- externalVar dph_Closure (fsLit "$:^")
      replicatePAIntPrimVar <- externalVar dph_Unboxed (fsLit "replicatePA_Int#")
      upToPAIntPrimVar <- externalVar dph_Unboxed (fsLit "upToPA_Int#")
      selectPAIntPrimVar <- externalVar dph_Unboxed (fsLit "selectPA_Int#")
      truesPABoolPrimVar <- externalVar dph_Unboxed (fsLit "truesPA_Bool#")
      lengthPAVar      <- externalVar dph_PArray (fsLit "lengthPA#")
      replicatePAVar   <- externalVar dph_PArray (fsLit "replicatePA#")
      emptyPAVar       <- externalVar dph_PArray (fsLit "emptyPA")
      packPAVar        <- externalVar dph_PArray (fsLit "packPA#")

      combines <- mapM (externalVar dph_PArray)
                       [mkFastString ("combine" ++ show i ++ "PA#")
                          | i <- [2..mAX_DPH_COMBINE]]
      let combinePAVars = listArray (2, mAX_DPH_COMBINE) combines

      liftingContext <- liftM (\u -> mkSysLocal (fsLit "lc") u intPrimTy)
                              newUnique

      return $ Builtins {
                 dphModules       = modules
               , parrayTyCon      = parrayTyCon
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
  where
    modules@(Modules {
               dph_PArray         = dph_PArray
             , dph_Repr           = dph_Repr
             , dph_Closure        = dph_Closure
             , dph_Unboxed        = dph_Unboxed
             })
      = dph_Modules pkg


initBuiltinVars :: Builtins -> DsM [(Var, Var)]
initBuiltinVars (Builtins { dphModules = mods })
  = do
      uvars <- zipWithM externalVar umods ufs
      vvars <- zipWithM externalVar vmods vfs
      cvars <- zipWithM externalVar cmods cfs
      return $ [(v,v) | v <- map dataConWorkId defaultDataConWorkers]
               ++ zip (map dataConWorkId cons) cvars
               ++ zip uvars vvars
  where
    (umods, ufs, vmods, vfs) = unzip4 (preludeVars mods)

    (cons, cmods, cfs) = unzip3 (preludeDataCons mods)

defaultDataConWorkers :: [DataCon]
defaultDataConWorkers = [trueDataCon, falseDataCon, unitDataCon]

preludeDataCons :: Modules -> [(DataCon, Module, FastString)]
preludeDataCons (Modules { dph_Prelude_Tuple = dph_Prelude_Tuple })
  = [mk_tup n dph_Prelude_Tuple (mkFastString $ "tup" ++ show n) | n <- [2..3]]
  where
    mk_tup n mod name = (tupleCon Boxed n, mod, name)

preludeVars :: Modules -> [(Module, FastString, Module, FastString)]
preludeVars (Modules { dph_Combinators    = dph_Combinators
                     , dph_PArray         = dph_PArray
                     , dph_Prelude_Int    = dph_Prelude_Int
                     , dph_Prelude_Double = dph_Prelude_Double
                     , dph_Prelude_Bool   = dph_Prelude_Bool 
                     , dph_Prelude_PArr   = dph_Prelude_PArr
                     })
  = [
      mk gHC_PARR (fsLit "mapP")       dph_Combinators (fsLit "mapPA")
    , mk gHC_PARR (fsLit "zipWithP")   dph_Combinators (fsLit "zipWithPA")
    , mk gHC_PARR (fsLit "zipP")       dph_Combinators (fsLit "zipPA")
    , mk gHC_PARR (fsLit "unzipP")     dph_Combinators (fsLit "unzipPA")
    , mk gHC_PARR (fsLit "filterP")    dph_Combinators (fsLit "filterPA")
    , mk gHC_PARR (fsLit "lengthP")    dph_Combinators (fsLit "lengthPA")
    , mk gHC_PARR (fsLit "replicateP") dph_Combinators (fsLit "replicatePA")
    , mk gHC_PARR (fsLit "!:")         dph_Combinators (fsLit "indexPA")
    , mk gHC_PARR (fsLit "crossMapP")  dph_Combinators (fsLit "crossMapPA")
    , mk gHC_PARR (fsLit "singletonP") dph_Combinators (fsLit "singletonPA")
    , mk gHC_PARR (fsLit "concatP")    dph_Combinators (fsLit "concatPA")
    , mk gHC_PARR (fsLit "+:+")        dph_Combinators (fsLit "appPA")
    , mk gHC_PARR (fsLit "emptyP")     dph_PArray (fsLit "emptyPA")

    , mk dph_Prelude_Int  (fsLit "plus") dph_Prelude_Int (fsLit "plusV")
    , mk dph_Prelude_Int  (fsLit "minus") dph_Prelude_Int (fsLit "minusV")
    , mk dph_Prelude_Int  (fsLit "mult")  dph_Prelude_Int (fsLit "multV")
    , mk dph_Prelude_Int  (fsLit "intDiv")  dph_Prelude_Int (fsLit "intDivV")
    , mk dph_Prelude_Int  (fsLit "intMod")  dph_Prelude_Int (fsLit "intModV")
    , mk dph_Prelude_Int  (fsLit "intSquareRoot")  dph_Prelude_Int (fsLit "intSquareRootV")
    , mk dph_Prelude_Int  (fsLit "intSumP")  dph_Prelude_Int (fsLit "intSumPA")
    , mk dph_Prelude_Int  (fsLit "enumFromToP")  dph_Prelude_Int (fsLit "enumFromToPA")
    , mk dph_Prelude_Int  (fsLit "upToP") dph_Prelude_Int (fsLit "upToPA")

    , mk dph_Prelude_Int  (fsLit "eq") dph_Prelude_Int (fsLit "eqV")
    , mk dph_Prelude_Int  (fsLit "neq") dph_Prelude_Int (fsLit "neqV")
    , mk dph_Prelude_Int  (fsLit "le")  dph_Prelude_Int (fsLit "leV")
    , mk dph_Prelude_Int  (fsLit "lt") dph_Prelude_Int (fsLit "ltV")
    , mk dph_Prelude_Int  (fsLit "ge") dph_Prelude_Int (fsLit "geV")
    , mk dph_Prelude_Int  (fsLit "gt")  dph_Prelude_Int (fsLit "gtV")

    , mk dph_Prelude_Double  (fsLit "plus") dph_Prelude_Double (fsLit "plusV")
    , mk dph_Prelude_Double  (fsLit "minus") dph_Prelude_Double (fsLit "minusV")
    , mk dph_Prelude_Double  (fsLit "mult")  dph_Prelude_Double (fsLit "multV")
    , mk dph_Prelude_Double  (fsLit "divide")  dph_Prelude_Double (fsLit "divideV")
    , mk dph_Prelude_Double  (fsLit  "squareRoot")  dph_Prelude_Double (fsLit "squareRootV")    
    , mk dph_Prelude_Double  (fsLit "doubleSumP")  dph_Prelude_Double (fsLit "doubleSumPA")
    , mk dph_Prelude_Double  (fsLit "minIndexP") 
         dph_Prelude_Double  (fsLit "minIndexPA")
    , mk dph_Prelude_Double  (fsLit "maxIndexP")
         dph_Prelude_Double  (fsLit "maxIndexPA")

    , mk dph_Prelude_Double  (fsLit "eq") dph_Prelude_Double (fsLit "eqV")
    , mk dph_Prelude_Double  (fsLit "neq") dph_Prelude_Double (fsLit "neqV")
    , mk dph_Prelude_Double  (fsLit "le")  dph_Prelude_Double (fsLit "leV")
    , mk dph_Prelude_Double  (fsLit "lt") dph_Prelude_Double (fsLit "ltV")
    , mk dph_Prelude_Double  (fsLit "ge") dph_Prelude_Double (fsLit "geV")
    , mk dph_Prelude_Double  (fsLit "gt")  dph_Prelude_Double (fsLit "gtV")

    , mk dph_Prelude_Bool  (fsLit "andP")  dph_Prelude_Bool (fsLit "andPA")
    , mk dph_Prelude_Bool  (fsLit "orP")  dph_Prelude_Bool (fsLit "orPA")

    -- FIXME: temporary
    , mk dph_Prelude_PArr (fsLit "fromPArrayP") dph_Prelude_PArr (fsLit "fromPArrayPA")
    , mk dph_Prelude_PArr (fsLit "toPArrayP") dph_Prelude_PArr (fsLit "toPArrayPA")
    , mk dph_Prelude_PArr (fsLit "fromNestedPArrayP") dph_Prelude_PArr (fsLit "fromNestedPArrayPA")
    , mk dph_Prelude_PArr (fsLit "combineP")    dph_Combinators (fsLit "combine2PA")
    ]
  where
    mk = (,,,)

initBuiltinTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinTyCons bi
  = do
      -- parr <- externalTyCon dph_Prelude_PArr (fsLit "PArr")
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
builtinPAs bi@(Builtins { dphModules = mods })
  = [
      mk (tyConName $ closureTyCon bi)  (dph_Closure   mods) (fsLit "dPA_Clo")
    , mk (tyConName $ voidTyCon bi)     (dph_Repr      mods) (fsLit "dPA_Void")
    , mk (tyConName $ parrayTyCon bi)   (dph_Instances mods) (fsLit "dPA_PArray")
    , mk unitTyConName                  (dph_Instances mods) (fsLit "dPA_Unit")

    , mk intTyConName                   (dph_Instances mods) (fsLit "dPA_Int")
    , mk doubleTyConName                (dph_Instances mods) (fsLit "dPA_Double")
    , mk boolTyConName                  (dph_Instances mods) (fsLit "dPA_Bool")
    ]
    ++ tups
  where
    mk name mod fs = (name, mod, fs)

    tups = map mk_tup [2..mAX_DPH_PROD]
    mk_tup n = mk (tyConName $ tupleTyCon Boxed n)
                  (dph_Instances mods)
                  (mkFastString $ "dPA_" ++ show n)

initBuiltinPRs :: Builtins -> DsM [(Name, Var)]
initBuiltinPRs = initBuiltinDicts . builtinPRs

builtinPRs :: Builtins -> [(Name, Module, FastString)]
builtinPRs bi@(Builtins { dphModules = mods }) =
  [
    mk (tyConName   unitTyCon)           (dph_Repr mods)    (fsLit "dPR_Unit")
  , mk (tyConName $ voidTyCon        bi) (dph_Repr mods)    (fsLit "dPR_Void")
  , mk (tyConName $ wrapTyCon        bi) (dph_Repr mods)    (fsLit "dPR_Wrap")
  , mk (tyConName $ enumerationTyCon bi) (dph_Repr mods)    (fsLit "dPR_Enumeration")
  , mk (tyConName $ closureTyCon     bi) (dph_Closure mods) (fsLit "dPR_Clo")

    -- temporary
  , mk intTyConName          (dph_Instances mods) (fsLit "dPR_Int")
  , mk doubleTyConName       (dph_Instances mods) (fsLit "dPR_Double")
  ]

  ++ map mk_sum  [2..mAX_DPH_SUM]
  ++ map mk_prod [2..mAX_DPH_PROD]
  where
    mk name mod fs = (name, mod, fs)

    mk_sum n = (tyConName $ sumTyCon n bi, dph_Repr mods,
                mkFastString ("dPR_Sum" ++ show n))

    mk_prod n = (tyConName $ prodTyCon n bi, dph_Repr mods,
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
  = dsLookupTyCon =<< lookupOrig mod (mkTcOccFS fs)

externalType :: Module -> FastString -> DsM Type
externalType mod fs
  = do
      tycon <- externalTyCon mod fs
      return $ mkTyConApp tycon []

unitTyConName :: Name
unitTyConName = tyConName unitTyCon


primMethod :: TyCon -> String -> Builtins -> DsM (Maybe Var)
primMethod  tycon method (Builtins { dphModules = mods })
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupGlobalId =<< lookupOrig (dph_Unboxed mods)
                                    (mkVarOcc $ method ++ suffix)

  | otherwise = return Nothing

primPArray :: TyCon -> Builtins -> DsM (Maybe TyCon)
primPArray tycon (Builtins { dphModules = mods })
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupTyCon =<< lookupOrig (dph_Unboxed mods)
                                 (mkTcOcc $ "PArray" ++ suffix)

  | otherwise = return Nothing

prim_ty_cons :: NameEnv String
prim_ty_cons = mkNameEnv [mk_prim intPrimTyCon]
  where
    mk_prim tycon = (tyConName tycon, '_' : getOccString tycon)


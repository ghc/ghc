module VectBuiltIn (
  Builtins(..), sumTyCon, prodTyCon, prodDataCon,
  selTy, selReplicate, selPick, selElements,
  combinePDVar, scalarZip, closureCtrFun,
  initBuiltins, initBuiltinVars, initBuiltinTyCons, initBuiltinDataCons,
  initBuiltinPAs, initBuiltinPRs,
  initBuiltinBoxedTyCons, initBuiltinScalars,

  primMethod, primPArray
) where

import DsMonad
import IfaceEnv        ( lookupOrig )

import Module
import DataCon         ( DataCon, dataConName, dataConWorkId )
import TyCon           ( TyCon, tyConName, tyConDataCons )
import Class           ( Class )
import CoreSyn         ( CoreExpr, Expr(..) )
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
import PrelNames       ( word8TyConName, gHC_PARR )
import BasicTypes      ( Boxity(..) )

import FastString
import Outputable

import Data.Array
import Control.Monad   ( liftM, zipWithM )
import Data.List       ( unzip4 )

mAX_DPH_PROD :: Int
mAX_DPH_PROD = 5

mAX_DPH_SUM :: Int
mAX_DPH_SUM = 2

mAX_DPH_COMBINE :: Int
mAX_DPH_COMBINE = 2

mAX_DPH_SCALAR_ARGS :: Int
mAX_DPH_SCALAR_ARGS = 3

data Modules = Modules {
                   dph_PArray :: Module
                 , dph_Repr :: Module
                 , dph_Closure :: Module
                 , dph_Unboxed :: Module
                 , dph_Instances :: Module
                 , dph_Combinators :: Module
                 , dph_Scalar :: Module
                 , dph_Selector :: Module
                 , dph_Prelude_PArr :: Module
                 , dph_Prelude_Int :: Module
                 , dph_Prelude_Word8 :: Module
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
  , dph_Scalar         = mk (fsLit "Data.Array.Parallel.Lifted.Scalar")
  , dph_Selector       = mk (fsLit "Data.Array.Parallel.Lifted.Selector")

  , dph_Prelude_PArr   = mk (fsLit "Data.Array.Parallel.Prelude.Base.PArr")
  , dph_Prelude_Int    = mk (fsLit "Data.Array.Parallel.Prelude.Base.Int")
  , dph_Prelude_Word8  = mk (fsLit "Data.Array.Parallel.Prelude.Base.Word8")
  , dph_Prelude_Double = mk (fsLit "Data.Array.Parallel.Prelude.Base.Double")
  , dph_Prelude_Bool   = mk (fsLit "Data.Array.Parallel.Prelude.Base.Bool")
  , dph_Prelude_Tuple  = mk (fsLit "Data.Array.Parallel.Prelude.Base.Tuple")
  }
  where
    mk = mkModule pkg . mkModuleNameFS


data Builtins = Builtins {
                  dphModules       :: Modules
                , parrayTyCon      :: TyCon
                , parrayDataCon    :: DataCon
                , pdataTyCon       :: TyCon
                , paTyCon          :: TyCon
                , paDataCon        :: DataCon
                , preprTyCon       :: TyCon
                , prTyCon          :: TyCon
                , prDataCon        :: DataCon
                , voidTyCon        :: TyCon
                , wrapTyCon        :: TyCon
                , selTys           :: Array Int Type
                , selReplicates    :: Array Int CoreExpr
                , selPicks         :: Array Int CoreExpr
                , selEls           :: Array (Int, Int) CoreExpr
                , sumTyCons        :: Array Int TyCon
                , closureTyCon     :: TyCon
                , voidVar          :: Var
                , pvoidVar         :: Var
                , punitVar         :: Var
                , mkPRVar          :: Var
                , closureVar       :: Var
                , applyVar         :: Var
                , liftedClosureVar :: Var
                , liftedApplyVar   :: Var
                , replicatePDVar   :: Var
                , emptyPDVar       :: Var
                , packPDVar        :: Var
                , combinePDVars    :: Array Int Var
                , scalarClass      :: Class
                , scalarZips       :: Array Int Var
                , closureCtrFuns   :: Array Int Var
                , liftingContext   :: Var
                }

indexBuiltin :: (Ix i, Outputable i) => String -> (Builtins -> Array i a)
                                        -> i -> Builtins -> a
indexBuiltin fn f i bi
  | inRange (bounds xs) i = xs ! i
  | otherwise = pprPanic fn (ppr i)
  where
    xs = f bi

selTy :: Int -> Builtins -> Type
selTy = indexBuiltin "selTy" selTys

selReplicate :: Int -> Builtins -> CoreExpr
selReplicate = indexBuiltin "selReplicate" selReplicates 

selPick :: Int -> Builtins -> CoreExpr
selPick = indexBuiltin "selPick" selPicks

selElements :: Int -> Int -> Builtins -> CoreExpr
selElements i j = indexBuiltin "selElements" selEls (i,j)

sumTyCon :: Int -> Builtins -> TyCon
sumTyCon = indexBuiltin "sumTyCon" sumTyCons

prodTyCon :: Int -> Builtins -> TyCon
prodTyCon n bi
  | n == 1                      = wrapTyCon bi
  | n >= 0 && n <= mAX_DPH_PROD = tupleTyCon Boxed n
  | otherwise = pprPanic "prodTyCon" (ppr n)

prodDataCon :: Int -> Builtins -> DataCon
prodDataCon n bi = case tyConDataCons (prodTyCon n bi) of
                     [con] -> con
                     _     -> pprPanic "prodDataCon" (ppr n)

combinePDVar :: Int -> Builtins -> Var
combinePDVar = indexBuiltin "combinePDVar" combinePDVars

scalarZip :: Int -> Builtins -> Var
scalarZip = indexBuiltin "scalarZip" scalarZips

closureCtrFun :: Int -> Builtins -> Var
closureCtrFun = indexBuiltin "closureCtrFun" closureCtrFuns

initBuiltins :: PackageId -> DsM Builtins
initBuiltins pkg
  = do
      parrayTyCon  <- externalTyCon dph_PArray (fsLit "PArray")
      let [parrayDataCon] = tyConDataCons parrayTyCon
      pdataTyCon   <- externalTyCon dph_PArray (fsLit "PData")
      paTyCon      <- externalTyCon dph_PArray (fsLit "PA")
      let [paDataCon] = tyConDataCons paTyCon
      preprTyCon   <- externalTyCon dph_PArray (fsLit "PRepr")
      prTyCon      <- externalTyCon dph_PArray (fsLit "PR")
      let [prDataCon] = tyConDataCons prTyCon
      closureTyCon <- externalTyCon dph_Closure (fsLit ":->")

      voidTyCon    <- externalTyCon dph_Repr (fsLit "Void")
      wrapTyCon    <- externalTyCon dph_Repr (fsLit "Wrap")
      sel_tys      <- mapM (externalType dph_Selector)
                           (numbered "Sel" 2 mAX_DPH_SUM)
      sel_replicates <- mapM (externalFun dph_Selector)
                             (numbered "replicate" 2 mAX_DPH_SUM)
      sel_picks    <- mapM (externalFun dph_Selector)
                           (numbered "pick" 2 mAX_DPH_SUM)
      sel_els      <- mapM mk_elements
                           [(i,j) | i <- [2..mAX_DPH_SUM], j <- [0..i-1]]
      sum_tcs      <- mapM (externalTyCon dph_Repr)
                           (numbered "Sum" 2 mAX_DPH_SUM)

      let selTys        = listArray (2, mAX_DPH_SUM) sel_tys
          selReplicates = listArray (2, mAX_DPH_SUM) sel_replicates
          selPicks      = listArray (2, mAX_DPH_SUM) sel_picks
          selEls        = array ((2,0), (mAX_DPH_SUM, mAX_DPH_SUM)) sel_els
          sumTyCons     = listArray (2, mAX_DPH_SUM) sum_tcs

      voidVar          <- externalVar dph_Repr (fsLit "void")
      pvoidVar         <- externalVar dph_Repr (fsLit "pvoid")
      punitVar         <- externalVar dph_Repr (fsLit "punit")
      mkPRVar          <- externalVar dph_PArray (fsLit "mkPR")
      closureVar       <- externalVar dph_Closure (fsLit "closure")
      applyVar         <- externalVar dph_Closure (fsLit "$:")
      liftedClosureVar <- externalVar dph_Closure (fsLit "liftedClosure")
      liftedApplyVar   <- externalVar dph_Closure (fsLit "liftedApply")
      replicatePDVar   <- externalVar dph_PArray (fsLit "replicatePD")
      emptyPDVar       <- externalVar dph_PArray (fsLit "emptyPD")
      packPDVar        <- externalVar dph_PArray (fsLit "packPD")

      combines <- mapM (externalVar dph_PArray)
                       [mkFastString ("combine" ++ show i ++ "PD")
                          | i <- [2..mAX_DPH_COMBINE]]
      let combinePDVars = listArray (2, mAX_DPH_COMBINE) combines

      scalarClass <- externalClass dph_Scalar (fsLit "Scalar")
      scalar_map <- externalVar dph_Scalar (fsLit "scalar_map")
      scalar_zip2 <- externalVar dph_Scalar (fsLit "scalar_zipWith")
      scalar_zips <- mapM (externalVar dph_Scalar)
                          (numbered "scalar_zipWith" 3 mAX_DPH_SCALAR_ARGS)
      let scalarZips = listArray (1, mAX_DPH_SCALAR_ARGS)
                                 (scalar_map : scalar_zip2 : scalar_zips)
      closures <- mapM (externalVar dph_Closure)
                       (numbered "closure" 1 mAX_DPH_SCALAR_ARGS)
      let closureCtrFuns = listArray (1, mAX_DPH_COMBINE) closures

      liftingContext <- liftM (\u -> mkSysLocal (fsLit "lc") u intPrimTy)
                              newUnique

      return $ Builtins {
                 dphModules       = modules
               , parrayTyCon      = parrayTyCon
               , parrayDataCon    = parrayDataCon
               , pdataTyCon       = pdataTyCon
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , preprTyCon       = preprTyCon
               , prTyCon          = prTyCon
               , prDataCon        = prDataCon
               , voidTyCon        = voidTyCon
               , wrapTyCon        = wrapTyCon
               , selTys           = selTys
               , selReplicates    = selReplicates
               , selPicks         = selPicks
               , selEls           = selEls
               , sumTyCons        = sumTyCons
               , closureTyCon     = closureTyCon
               , voidVar          = voidVar
               , pvoidVar         = pvoidVar
               , punitVar         = punitVar
               , mkPRVar          = mkPRVar
               , closureVar       = closureVar
               , applyVar         = applyVar
               , liftedClosureVar = liftedClosureVar
               , liftedApplyVar   = liftedApplyVar
               , replicatePDVar   = replicatePDVar
               , emptyPDVar       = emptyPDVar
               , packPDVar        = packPDVar
               , combinePDVars    = combinePDVars
               , scalarClass      = scalarClass
               , scalarZips       = scalarZips
               , closureCtrFuns   = closureCtrFuns
               , liftingContext   = liftingContext
               }
  where
    modules@(Modules {
               dph_PArray         = dph_PArray
             , dph_Repr           = dph_Repr
             , dph_Closure        = dph_Closure
             , dph_Selector       = dph_Selector
             , dph_Scalar         = dph_Scalar
             })
      = dph_Modules pkg

    numbered :: String -> Int -> Int -> [FastString]
    numbered pfx m n = [mkFastString (pfx ++ show i) | i <- [m..n]]

    mk_elements :: (Int, Int) -> DsM ((Int, Int), CoreExpr)
    mk_elements (i,j)
      = do
          v <- externalVar dph_Selector
             $ mkFastString ("elementsSel" ++ show i ++ "_" ++ show j ++ "#")
          return ((i,j), Var v)


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
                     , dph_Prelude_Word8  = dph_Prelude_Word8
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

    , mk' dph_Prelude_Int "div"  "divV"
    , mk' dph_Prelude_Int "mod"  "modV"
    , mk' dph_Prelude_Int "sqrt" "sqrtV"
    , mk' dph_Prelude_Int "enumFromToP" "enumFromToPA"
    -- , mk' dph_Prelude_Int "upToP" "upToPA"
    ]
    ++ vars_Ord dph_Prelude_Int
    ++ vars_Num dph_Prelude_Int

    ++ vars_Ord dph_Prelude_Word8
    ++ vars_Num dph_Prelude_Word8
    ++
    [ mk' dph_Prelude_Word8 "div" "divV"
    , mk' dph_Prelude_Word8 "mod" "modV"
    , mk' dph_Prelude_Word8 "fromInt" "fromIntV"
    , mk' dph_Prelude_Word8 "toInt" "toIntV"
    ]

    ++ vars_Ord dph_Prelude_Double
    ++ vars_Num dph_Prelude_Double
    ++ vars_Fractional dph_Prelude_Double
    ++ vars_Floating dph_Prelude_Double
    ++ vars_RealFrac dph_Prelude_Double
    ++
    [ mk dph_Prelude_Bool  (fsLit "andP")  dph_Prelude_Bool (fsLit "andPA")
    , mk dph_Prelude_Bool  (fsLit "orP")  dph_Prelude_Bool (fsLit "orPA")

    -- FIXME: temporary
    , mk dph_Prelude_PArr (fsLit "fromPArrayP") dph_Prelude_PArr (fsLit "fromPArrayPA")
    , mk dph_Prelude_PArr (fsLit "toPArrayP") dph_Prelude_PArr (fsLit "toPArrayPA")
    , mk dph_Prelude_PArr (fsLit "fromNestedPArrayP") dph_Prelude_PArr (fsLit "fromNestedPArrayPA")
    , mk dph_Prelude_PArr (fsLit "combineP")    dph_Combinators (fsLit "combine2PA")
    ]
  where
    mk  = (,,,)
    mk' mod v v' = mk mod (fsLit v) mod (fsLit v')

    vars_Ord mod = [mk' mod "=="  "eqV"
                   ,mk' mod "/=" "neqV"
                   ,mk' mod "<="  "leV"
                   ,mk' mod "<"   "ltV"
                   ,mk' mod ">="  "geV"
                   ,mk' mod ">"   "gtV"
                   ,mk' mod "min" "minV"
                   ,mk' mod "max" "maxV"
                   ,mk' mod "minimumP" "minimumPA"
                   ,mk' mod "maximumP" "maximumPA"
                   ,mk' mod "minIndexP" "minIndexPA"
                   ,mk' mod "maxIndexP" "maxIndexPA"
                   ]

    vars_Num mod = [mk' mod "+"        "plusV"
                   ,mk' mod "-"        "minusV"
                   ,mk' mod "*"        "multV"
                   ,mk' mod "negate"   "negateV"
                   ,mk' mod "abs"      "absV"
                   ,mk' mod "sumP"     "sumPA"
                   ,mk' mod "productP" "productPA"
                   ]

    vars_Fractional mod = [mk' mod "/"     "divideV"
                          ,mk' mod "recip" "recipV"
                          ]

    vars_Floating mod = [mk' mod "pi" "pi"
                        ,mk' mod "exp" "expV"
                        ,mk' mod "sqrt" "sqrtV"
                        ,mk' mod "log" "logV"
                        ,mk' mod "sin" "sinV"
                        ,mk' mod "tan" "tanV"
                        ,mk' mod "cos" "cosV"
                        ,mk' mod "asin" "asinV"
                        ,mk' mod "atan" "atanV"
                        ,mk' mod "acos" "acosV"
                        ,mk' mod "sinh" "sinhV"
                        ,mk' mod "tanh" "tanhV"
                        ,mk' mod "cosh" "coshV"
                        ,mk' mod "asinh" "asinhV"
                        ,mk' mod "atanh" "atanhV"
                        ,mk' mod "acosh" "acoshV"
                        ,mk' mod "**"    "powV"
                        ,mk' mod "logBase" "logBaseV"
                        ]

    vars_RealFrac mod = [mk' mod "fromInt" "fromIntV"
                        ,mk' mod "truncate" "truncateV"
                        ,mk' mod "round" "roundV"
                        ,mk' mod "ceiling" "ceilingV"
                        ,mk' mod "floor" "floorV"
                        ]

initBuiltinTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinTyCons bi
  = do
      -- parr <- externalTyCon dph_Prelude_PArr (fsLit "PArr")
      dft_tcs <- defaultTyCons
      return $ (tyConName funTyCon, closureTyCon bi)
             : (parrTyConName,      parrayTyCon bi)

             -- FIXME: temporary
             : (tyConName $ parrayTyCon bi, parrayTyCon bi)

             : [(tyConName tc, tc) | tc <- dft_tcs]

defaultTyCons :: DsM [TyCon]
defaultTyCons
  = do
      word8 <- dsLookupTyCon word8TyConName
      return [intTyCon, boolTyCon, doubleTyCon, word8]

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
    , mk word8TyConName                 (dph_Instances mods) (fsLit "dPA_Word8")
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
  , mk (tyConName $ closureTyCon     bi) (dph_Closure mods) (fsLit "dPR_Clo")

    -- temporary
  , mk intTyConName          (dph_Instances mods) (fsLit "dPR_Int")
  , mk word8TyConName        (dph_Instances mods) (fsLit "dPR_Word8")
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


initBuiltinScalars :: Builtins -> DsM [Var]
initBuiltinScalars bi
  = mapM (uncurry externalVar) (preludeScalars $ dphModules bi)


preludeScalars :: Modules -> [(Module, FastString)]
preludeScalars (Modules { dph_Prelude_Int    = dph_Prelude_Int
                        , dph_Prelude_Word8  = dph_Prelude_Word8
                        , dph_Prelude_Double = dph_Prelude_Double
                        })
  = [
      mk dph_Prelude_Int "div"
    , mk dph_Prelude_Int "mod"
    , mk dph_Prelude_Int "sqrt"
    ]
    ++ scalars_Ord dph_Prelude_Int
    ++ scalars_Num dph_Prelude_Int

    ++ scalars_Ord dph_Prelude_Word8
    ++ scalars_Num dph_Prelude_Word8
    ++
    [ mk dph_Prelude_Word8 "div"
    , mk dph_Prelude_Word8 "mod"
    , mk dph_Prelude_Word8 "fromInt"
    , mk dph_Prelude_Word8 "toInt"
    ]

    ++ scalars_Ord dph_Prelude_Double
    ++ scalars_Num dph_Prelude_Double
    ++ scalars_Fractional dph_Prelude_Double
    ++ scalars_Floating dph_Prelude_Double
    ++ scalars_RealFrac dph_Prelude_Double
  where
    mk mod s = (mod, fsLit s)

    scalars_Ord mod = [mk mod "=="
                      ,mk mod "/="
                      ,mk mod "<="
                      ,mk mod "<"
                      ,mk mod ">="
                      ,mk mod ">"
                      ,mk mod "min"
                      ,mk mod "max"
                      ]

    scalars_Num mod = [mk mod "+"
                      ,mk mod "-"
                      ,mk mod "*"
                      ,mk mod "negate"
                      ,mk mod "abs"
                      ]

    scalars_Fractional mod = [mk mod "/"
                             ,mk mod "recip"
                             ]

    scalars_Floating mod = [mk mod "pi"
                           ,mk mod "exp"
                           ,mk mod "sqrt"
                           ,mk mod "log"
                           ,mk mod "sin"
                           ,mk mod "tan"
                           ,mk mod "cos"
                           ,mk mod "asin"
                           ,mk mod "atan"
                           ,mk mod "acos"
                           ,mk mod "sinh"
                           ,mk mod "tanh"
                           ,mk mod "cosh"
                           ,mk mod "asinh"
                           ,mk mod "atanh"
                           ,mk mod "acosh"
                           ,mk mod "**"
                           ,mk mod "logBase"
                           ]

    scalars_RealFrac mod = [mk mod "fromInt"
                           ,mk mod "truncate"
                           ,mk mod "round"
                           ,mk mod "ceiling"
                           ,mk mod "floor"
                           ]


externalVar :: Module -> FastString -> DsM Var
externalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)

externalFun :: Module -> FastString -> DsM CoreExpr
externalFun mod fs
  = do
      var <- externalVar mod fs
      return $ Var var

externalTyCon :: Module -> FastString -> DsM TyCon
externalTyCon mod fs
  = dsLookupTyCon =<< lookupOrig mod (mkTcOccFS fs)

externalType :: Module -> FastString -> DsM Type
externalType mod fs
  = do
      tycon <- externalTyCon mod fs
      return $ mkTyConApp tycon []

externalClass :: Module -> FastString -> DsM Class
externalClass mod fs
  = dsLookupClass =<< lookupOrig mod (mkTcOccFS fs)

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



-- | The vectoriser rewrites user code to use builtin types and functions exported by the DPH library.
--   We track the names of those things in the `Builtis` type, and provide selection functions 
--   to help extract their names.
module VectBuiltIn (
  Builtins(..),

  -- * Projections
  sumTyCon, prodTyCon, prodDataCon,
  selTy,selReplicate, selPick, selTags, selElements,
  combinePDVar, scalarZip, closureCtrFun,

  -- * Initialisation
  initBuiltins, initBuiltinVars, initBuiltinTyCons, initBuiltinDataCons,
  initBuiltinPAs, initBuiltinPRs,
  initBuiltinBoxedTyCons, initBuiltinScalars,

  primMethod, primPArray
) where

import Vectorise.Builtins.Modules
import Vectorise.Builtins.Base

import DsMonad
import IfaceEnv        ( lookupOrig )
import InstEnv

import Module
import DataCon         ( DataCon, dataConName, dataConWorkId )
import TyCon           ( TyCon, tyConName, tyConDataCons )
import Class           ( Class, classTyCon )
import CoreSyn         ( CoreExpr, Expr(..) )
import Var             ( Var )
import Id              ( mkSysLocal )
import Name            ( Name, getOccString )
import NameEnv
import OccName

import TypeRep         ( funTyCon )
import Type            ( Type, mkTyConApp )
import TysPrim
import TysWiredIn      ( unitDataCon,
                         tupleTyCon, tupleCon,
                         intTyCon,
                         doubleTyCon,
                         boolTyCon, trueDataCon, falseDataCon,
                         parrTyConName )
import PrelNames       ( word8TyConName, gHC_PARR, gHC_CLASSES )
import BasicTypes      ( Boxity(..) )

import FastString
import Outputable

import Data.Array
import Control.Monad   ( liftM, zipWithM )
import Data.List       ( unzip4 )




-- Initialisation -------------------------------------------------------------
-- | Create the initial map of builtin types and functions.
initBuiltins 
	:: PackageId 	-- ^ package id the builtins are in, eg dph-common
	-> DsM Builtins

initBuiltins pkg
  = do
      mapM_ load dph_Orphans

      -- From dph-common:Data.Array.Parallel.Lifted.PArray
      parrayTyCon	<- externalTyCon dph_PArray (fsLit "PArray")
      let [parrayDataCon] = tyConDataCons parrayTyCon
      pdataTyCon	<- externalTyCon dph_PArray (fsLit "PData")
      paTyCon		<- externalClassTyCon dph_PArray (fsLit "PA")
      let [paDataCon]	= tyConDataCons paTyCon
      preprTyCon	<- externalTyCon dph_PArray (fsLit "PRepr")
      prTyCon		<- externalClassTyCon dph_PArray (fsLit "PR")
      let [prDataCon]	= tyConDataCons prTyCon

      -- wher
      closureTyCon	<- externalTyCon dph_Closure (fsLit ":->")

      -- From dph-common:Data.Array.Parallel.Lifted.Repr
      voidTyCon		<- externalTyCon dph_Repr (fsLit "Void")
      wrapTyCon		<- externalTyCon dph_Repr (fsLit "Wrap")

      -- From dph-common:Data.Array.Parallel.Lifted.Unboxed
      sel_tys      <- mapM (externalType dph_Unboxed)
                           (numbered "Sel" 2 mAX_DPH_SUM)

      sel_replicates <- mapM (externalFun dph_Unboxed)
                             (numbered_hash "replicateSel" 2 mAX_DPH_SUM)

      sel_picks    <- mapM (externalFun dph_Unboxed)
                           (numbered_hash "pickSel" 2 mAX_DPH_SUM)

      sel_tags     <- mapM (externalFun dph_Unboxed)
                           (numbered "tagsSel" 2 mAX_DPH_SUM)

      sel_els      <- mapM mk_elements
                           [(i,j) | i <- [2..mAX_DPH_SUM], j <- [0..i-1]]

      sum_tcs      <- mapM (externalTyCon dph_Repr)
                           (numbered "Sum" 2 mAX_DPH_SUM)

      let selTys        = listArray (2, mAX_DPH_SUM) sel_tys
          selReplicates = listArray (2, mAX_DPH_SUM) sel_replicates
          selPicks      = listArray (2, mAX_DPH_SUM) sel_picks
          selTagss      = listArray (2, mAX_DPH_SUM) sel_tags
          selEls        = array ((2,0), (mAX_DPH_SUM, mAX_DPH_SUM)) sel_els
          sumTyCons     = listArray (2, mAX_DPH_SUM) sum_tcs


      voidVar          <- externalVar dph_Repr (fsLit "void")
      pvoidVar         <- externalVar dph_Repr (fsLit "pvoid")
      fromVoidVar      <- externalVar dph_Repr (fsLit "fromVoid")
      punitVar         <- externalVar dph_Repr (fsLit "punit")
      closureVar       <- externalVar dph_Closure (fsLit "closure")
      applyVar         <- externalVar dph_Closure (fsLit "$:")
      liftedClosureVar <- externalVar dph_Closure (fsLit "liftedClosure")
      liftedApplyVar   <- externalVar dph_Closure (fsLit "liftedApply")
      replicatePDVar   <- externalVar dph_PArray (fsLit "replicatePD")
      emptyPDVar       <- externalVar dph_PArray (fsLit "emptyPD")
      packByTagPDVar   <- externalVar dph_PArray (fsLit "packByTagPD")

      combines 		<- mapM (externalVar dph_PArray)
                       		[mkFastString ("combine" ++ show i ++ "PD")
                          		| i <- [2..mAX_DPH_COMBINE]]
      let combinePDVars = listArray (2, mAX_DPH_COMBINE) combines

      scalarClass 	<- externalClass dph_PArray (fsLit "Scalar")
      scalar_map	<- externalVar dph_Scalar (fsLit "scalar_map")
      scalar_zip2	<- externalVar dph_Scalar (fsLit "scalar_zipWith")
      scalar_zips	<- mapM (externalVar dph_Scalar)
                          	(numbered "scalar_zipWith" 3 mAX_DPH_SCALAR_ARGS)
      let scalarZips 	= listArray (1, mAX_DPH_SCALAR_ARGS)
                                 (scalar_map : scalar_zip2 : scalar_zips)
      closures 		<- mapM (externalVar dph_Closure)
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
               , selTagss         = selTagss
               , selEls           = selEls
               , sumTyCons        = sumTyCons
               , closureTyCon     = closureTyCon
               , voidVar          = voidVar
               , pvoidVar         = pvoidVar
               , fromVoidVar      = fromVoidVar
               , punitVar         = punitVar
               , closureVar       = closureVar
               , applyVar         = applyVar
               , liftedClosureVar = liftedClosureVar
               , liftedApplyVar   = liftedApplyVar
               , replicatePDVar   = replicatePDVar
               , emptyPDVar       = emptyPDVar
               , packByTagPDVar   = packByTagPDVar
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
             , dph_Scalar         = dph_Scalar
             , dph_Unboxed        = dph_Unboxed
             })
      = dph_Modules pkg

    load get_mod = dsLoadModule doc mod
      where
        mod = get_mod modules 
        doc = ppr mod <+> ptext (sLit "is a DPH module")

    -- Make a list of numbered strings in some range, eg foo3, foo4, foo5
    numbered :: String -> Int -> Int -> [FastString]
    numbered pfx m n = [mkFastString (pfx ++ show i) | i <- [m..n]]

    numbered_hash :: String -> Int -> Int -> [FastString]
    numbered_hash pfx m n = [mkFastString (pfx ++ show i ++ "#") | i <- [m..n]]

    mk_elements :: (Int, Int) -> DsM ((Int, Int), CoreExpr)
    mk_elements (i,j)
      = do
          v <- externalVar dph_Unboxed
             $ mkFastString ("elementsSel" ++ show i ++ "_" ++ show j ++ "#")
          return ((i,j), Var v)


-- | Get the mapping of names in the Prelude to names in the DPH library.
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
    (cons, cmods, cfs)       = unzip3 (preludeDataCons mods)

defaultDataConWorkers :: [DataCon]
defaultDataConWorkers = [trueDataCon, falseDataCon, unitDataCon]

preludeDataCons :: Modules -> [(DataCon, Module, FastString)]
preludeDataCons (Modules { dph_Prelude_Tuple = dph_Prelude_Tuple })
  = [mk_tup n dph_Prelude_Tuple (mkFastString $ "tup" ++ show n) | n <- [2..3]]
  where
    mk_tup n mod name = (tupleCon Boxed n, mod, name)


-- | Mapping of prelude functions to vectorised versions.
--     Functions like filterP currently have a working but naive version in GHC.PArr
--     During vectorisation we replace these by calls to filterPA, which are
--     defined in dph-common Data.Array.Parallel.Lifted.Combinators
--
--     As renamer only sees the GHC.PArr functions, if you want to add a new function
--     to the vectoriser there has to be a definition for it in GHC.PArr, even though
--     it will never be used at runtime.
--
preludeVars :: Modules -> [(Module, FastString, Module, FastString)]
preludeVars (Modules { dph_Combinators    = dph_Combinators
                     , dph_PArray         = dph_PArray
                     , dph_Prelude_Int    = dph_Prelude_Int
                     , dph_Prelude_Word8  = dph_Prelude_Word8
                     , dph_Prelude_Double = dph_Prelude_Double
                     , dph_Prelude_Bool   = dph_Prelude_Bool 
                     , dph_Prelude_PArr   = dph_Prelude_PArr
                     })

    -- Functions that work on whole PArrays, defined in GHC.PArr
  = [ mk gHC_PARR (fsLit "mapP")       dph_Combinators (fsLit "mapPA")
    , mk gHC_PARR (fsLit "zipWithP")   dph_Combinators (fsLit "zipWithPA")
    , mk gHC_PARR (fsLit "zipP")       dph_Combinators (fsLit "zipPA")
    , mk gHC_PARR (fsLit "unzipP")     dph_Combinators (fsLit "unzipPA")
    , mk gHC_PARR (fsLit "filterP")    dph_Combinators (fsLit "filterPA")
    , mk gHC_PARR (fsLit "lengthP")    dph_Combinators (fsLit "lengthPA")
    , mk gHC_PARR (fsLit "replicateP") dph_Combinators (fsLit "replicatePA")
    , mk gHC_PARR (fsLit "!:")         dph_Combinators (fsLit "indexPA")
    , mk gHC_PARR (fsLit "sliceP")     dph_Combinators (fsLit "slicePA")
    , mk gHC_PARR (fsLit "crossMapP")  dph_Combinators (fsLit "crossMapPA")
    , mk gHC_PARR (fsLit "singletonP") dph_Combinators (fsLit "singletonPA")
    , mk gHC_PARR (fsLit "concatP")    dph_Combinators (fsLit "concatPA")
    , mk gHC_PARR (fsLit "+:+")        dph_Combinators (fsLit "appPA")
    , mk gHC_PARR (fsLit "emptyP")     dph_PArray      (fsLit "emptyPA")

    -- Map scalar functions to versions using closures. 
    , mk' dph_Prelude_Int "div"         "divV"
    , mk' dph_Prelude_Int "mod"         "modV"
    , mk' dph_Prelude_Int "sqrt"        "sqrtV"
    , mk' dph_Prelude_Int "enumFromToP" "enumFromToPA"
    -- , mk' dph_Prelude_Int "upToP" "upToPA"
    ]
    ++ vars_Ord dph_Prelude_Int
    ++ vars_Num dph_Prelude_Int

    ++ vars_Ord dph_Prelude_Word8
    ++ vars_Num dph_Prelude_Word8
    ++
    [ mk' dph_Prelude_Word8 "div"     "divV"
    , mk' dph_Prelude_Word8 "mod"     "modV"
    , mk' dph_Prelude_Word8 "fromInt" "fromIntV"
    , mk' dph_Prelude_Word8 "toInt"   "toIntV"
    ]

    ++ vars_Ord        dph_Prelude_Double
    ++ vars_Num        dph_Prelude_Double
    ++ vars_Fractional dph_Prelude_Double
    ++ vars_Floating   dph_Prelude_Double
    ++ vars_RealFrac   dph_Prelude_Double
    ++
    [ mk dph_Prelude_Bool  (fsLit "andP")  dph_Prelude_Bool (fsLit "andPA")
    , mk dph_Prelude_Bool  (fsLit "orP")   dph_Prelude_Bool (fsLit "orPA")

    , mk gHC_CLASSES (fsLit "not")         dph_Prelude_Bool (fsLit "notV")
    , mk gHC_CLASSES (fsLit "&&")          dph_Prelude_Bool (fsLit "andV")
    , mk gHC_CLASSES (fsLit "||")          dph_Prelude_Bool (fsLit "orV")

    -- FIXME: temporary
    , mk dph_Prelude_PArr (fsLit "fromPArrayP")       dph_Prelude_PArr (fsLit "fromPArrayPA")
    , mk dph_Prelude_PArr (fsLit "toPArrayP")         dph_Prelude_PArr (fsLit "toPArrayPA")
    , mk dph_Prelude_PArr (fsLit "fromNestedPArrayP") dph_Prelude_PArr (fsLit "fromNestedPArrayPA")
    , mk dph_Prelude_PArr (fsLit "combineP")          dph_Combinators  (fsLit "combine2PA")
    , mk dph_Prelude_PArr (fsLit "updateP")           dph_Combinators  (fsLit "updatePA")
    , mk dph_Prelude_PArr (fsLit "bpermuteP")         dph_Combinators  (fsLit "bpermutePA")
    , mk dph_Prelude_PArr (fsLit "indexedP")          dph_Combinators  (fsLit "indexedPA")
    ]
  where
    mk  = (,,,)
    mk' mod v v' = mk mod (fsLit v) mod (fsLit v')

    vars_Ord mod 
     = [ mk' mod "=="        "eqV"
       , mk' mod "/="        "neqV"
       , mk' mod "<="        "leV"
       , mk' mod "<"         "ltV"
       , mk' mod ">="        "geV"
       , mk' mod ">"         "gtV"
       , mk' mod "min"       "minV"
       , mk' mod "max"       "maxV"
       , mk' mod "minimumP"  "minimumPA"
       , mk' mod "maximumP"  "maximumPA"
       , mk' mod "minIndexP" "minIndexPA"
       , mk' mod "maxIndexP" "maxIndexPA"
       ]

    vars_Num mod 
     = [ mk' mod "+"        "plusV"
       , mk' mod "-"        "minusV"
       , mk' mod "*"        "multV"
       , mk' mod "negate"   "negateV"
       , mk' mod "abs"      "absV"
       , mk' mod "sumP"     "sumPA"
       , mk' mod "productP" "productPA"
       ]

    vars_Fractional mod 
     = [ mk' mod "/"     "divideV"
       , mk' mod "recip" "recipV"
       ]

    vars_Floating mod 
     = [ mk' mod "pi"      "pi"
       , mk' mod "exp"     "expV"
       , mk' mod "sqrt"    "sqrtV"
       , mk' mod "log"     "logV"
       , mk' mod "sin"     "sinV"
       , mk' mod "tan"     "tanV"
       , mk' mod "cos"     "cosV"
       , mk' mod "asin"    "asinV"
       , mk' mod "atan"    "atanV"
       , mk' mod "acos"    "acosV"
       , mk' mod "sinh"    "sinhV"
       , mk' mod "tanh"    "tanhV"
       , mk' mod "cosh"    "coshV"
       , mk' mod "asinh"   "asinhV"
       , mk' mod "atanh"   "atanhV"
       , mk' mod "acosh"   "acoshV"
       , mk' mod "**"      "powV"
       , mk' mod "logBase" "logBaseV"
       ]

    vars_RealFrac mod
     = [ mk' mod "fromInt"  "fromIntV"
       , mk' mod "truncate" "truncateV"
       , mk' mod "round"    "roundV"
       , mk' mod "ceiling"  "ceilingV"
       , mk' mod "floor"    "floorV"
       ]


-- | Get a list of names to `TyCon`s in the mock prelude.
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


-- | Get a list of names to `DataCon`s in the mock prelude.
initBuiltinDataCons :: Builtins -> [(Name, DataCon)]
initBuiltinDataCons _ = [(dataConName dc, dc)| dc <- defaultDataCons]

defaultDataCons :: [DataCon]
defaultDataCons = [trueDataCon, falseDataCon, unitDataCon]


-- | Get the names of all buildin instance functions for the PA class.
initBuiltinPAs :: Builtins -> (InstEnv, InstEnv) -> DsM [(Name, Var)]
initBuiltinPAs (Builtins { dphModules = mods }) insts
  = liftM (initBuiltinDicts insts) (externalClass (dph_PArray mods) (fsLit "PA"))


-- | Get the names of all builtin instance functions for the PR class.
initBuiltinPRs :: Builtins -> (InstEnv, InstEnv) -> DsM [(Name, Var)]
initBuiltinPRs (Builtins { dphModules = mods }) insts
  = liftM (initBuiltinDicts insts) (externalClass (dph_PArray mods) (fsLit "PR"))


-- | Get the names of all DPH instance functions for this class.
initBuiltinDicts :: (InstEnv, InstEnv) -> Class -> [(Name, Var)]
initBuiltinDicts insts cls = map find $ classInstances insts cls
  where
    find i | [Just tc] <- instanceRoughTcs i 	= (tc, instanceDFunId i)
           | otherwise				= pprPanic "Invalid DPH instance" (ppr i)


-- | Get a list of boxed `TyCons` in the mock prelude. This is Int only.
initBuiltinBoxedTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinBoxedTyCons = return . builtinBoxedTyCons

builtinBoxedTyCons :: Builtins -> [(Name, TyCon)]
builtinBoxedTyCons _ 
  = [(tyConName intPrimTyCon, intTyCon)]


-- | Get a list of all scalar functions in the mock prelude.
initBuiltinScalars :: Builtins -> DsM [Var]
initBuiltinScalars bi
  = mapM (uncurry externalVar) (preludeScalars $ dphModules bi)


preludeScalars :: Modules -> [(Module, FastString)]
preludeScalars (Modules { dph_Prelude_Int    = dph_Prelude_Int
                        , dph_Prelude_Word8  = dph_Prelude_Word8
                        , dph_Prelude_Double = dph_Prelude_Double
                        })
  = [ mk dph_Prelude_Int "div"
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

    scalars_Ord mod 
     = [ mk mod "=="
       , mk mod "/="
       , mk mod "<="
       , mk mod "<"
       , mk mod ">="
       , mk mod ">"
       , mk mod "min"
       , mk mod "max"
       ]

    scalars_Num mod 
     = [ mk mod "+"
       , mk mod "-"
       , mk mod "*"
       , mk mod "negate"
       , mk mod "abs"
       ]

    scalars_Fractional mod 
     = [ mk mod "/"
       , mk mod "recip"
       ]

    scalars_Floating mod 
     = [ mk mod "pi"
       , mk mod "exp"
       , mk mod "sqrt"
       , mk mod "log"
       , mk mod "sin"
       , mk mod "tan"
       , mk mod "cos"
       , mk mod "asin"
       , mk mod "atan"
       , mk mod "acos"
       , mk mod "sinh"
       , mk mod "tanh"
       , mk mod "cosh"
       , mk mod "asinh"
       , mk mod "atanh"
       , mk mod "acosh"
       , mk mod "**"
       , mk mod "logBase"
       ]

    scalars_RealFrac mod 
     = [ mk mod "fromInt"
       , mk mod "truncate"
       , mk mod "round"
       , mk mod "ceiling"
       , mk mod "floor"
       ]


-- | Lookup some variable given its name and the module that contains it.
externalVar :: Module -> FastString -> DsM Var
externalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)


-- | Like `externalVar` but wrap the `Var` in a `CoreExpr`
externalFun :: Module -> FastString -> DsM CoreExpr
externalFun mod fs
 = do var <- externalVar mod fs
      return $ Var var


-- | Lookup some `TyCon` given its name and the module that contains it.
externalTyCon :: Module -> FastString -> DsM TyCon
externalTyCon mod fs
  = dsLookupTyCon =<< lookupOrig mod (mkTcOccFS fs)


-- | Lookup some `Type` given its name and the module that contains it.
externalType :: Module -> FastString -> DsM Type
externalType mod fs
 = do  tycon <- externalTyCon mod fs
       return $ mkTyConApp tycon []


-- | Lookup some `Class` given its name and the module that contains it.
externalClass :: Module -> FastString -> DsM Class
externalClass mod fs
  = dsLookupClass =<< lookupOrig mod (mkClsOccFS fs)


-- | Like `externalClass`, but get the TyCon of of the class.
externalClassTyCon :: Module -> FastString -> DsM TyCon
externalClassTyCon mod fs = liftM classTyCon (externalClass mod fs)


-- | Lookup a method function given its name and instance type.
primMethod :: TyCon -> String -> Builtins -> DsM (Maybe Var)
primMethod  tycon method (Builtins { dphModules = mods })
  | Just suffix <- lookupNameEnv prim_ty_cons (tyConName tycon)
  = liftM Just
  $ dsLookupGlobalId =<< lookupOrig (dph_Unboxed mods)
                                    (mkVarOcc $ method ++ suffix)

  | otherwise = return Nothing

-- | Lookup the representation type we use for PArrays that contain a given element type.
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


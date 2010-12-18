

module Vectorise.Builtins.Initialise (
	-- * Initialisation
	initBuiltins, initBuiltinVars, initBuiltinTyCons, initBuiltinDataCons,
	initBuiltinPAs, initBuiltinPRs,
	initBuiltinBoxedTyCons, initBuiltinScalars,
) where
import Vectorise.Builtins.Base
import Vectorise.Builtins.Modules
import Vectorise.Builtins.Prelude

import BasicTypes
import PrelNames
import TysPrim
import DsMonad
import IfaceEnv
import InstEnv
import TysWiredIn
import DataCon
import TyCon
import Class
import CoreSyn
import Type
import Name
import Module
import Var
import Id
import FastString
import Outputable

import Control.Monad
import Data.Array
import Data.List

-- | Create the initial map of builtin types and functions.
initBuiltins 
	:: PackageId 	-- ^ package id the builtins are in, eg dph-common
	-> DsM Builtins

initBuiltins pkg
 = do mapM_ load dph_Orphans

      -- From dph-common:Data.Array.Parallel.Lifted.PArray
      parrayTyCon	<- externalTyCon	dph_PArray	(fsLit "PArray")
      let [parrayDataCon] = tyConDataCons parrayTyCon

      pdataTyCon	<- externalTyCon	dph_PArray	(fsLit "PData")
      pa                <- externalClass        dph_PArray      (fsLit "PA")
      let paTyCon     = classTyCon pa
          [paDataCon] = tyConDataCons paTyCon
          paPRSel     = classSCSelId pa 0

      preprTyCon	<- externalTyCon 	dph_PArray	(fsLit "PRepr")
      prTyCon		<- externalClassTyCon	dph_PArray	(fsLit "PR")
      let [prDataCon]	= tyConDataCons prTyCon

      closureTyCon	<- externalTyCon dph_Closure		(fsLit ":->")

      -- From dph-common:Data.Array.Parallel.Lifted.Repr
      voidTyCon		<- externalTyCon	dph_Repr	(fsLit "Void")
      wrapTyCon		<- externalTyCon	dph_Repr	(fsLit "Wrap")

      -- From dph-common:Data.Array.Parallel.Lifted.Unboxed
      sel_tys		<- mapM (externalType dph_Unboxed)
                           	(numbered "Sel" 2 mAX_DPH_SUM)

      sel_replicates	<- mapM (externalFun dph_Unboxed)
				(numbered_hash "replicateSel" 2 mAX_DPH_SUM)

      sel_picks 	<- mapM (externalFun dph_Unboxed)
				(numbered_hash "pickSel" 2 mAX_DPH_SUM)

      sel_tags		<- mapM (externalFun dph_Unboxed)
				(numbered "tagsSel" 2 mAX_DPH_SUM)

      sel_els		<- mapM mk_elements
				[(i,j) | i <- [2..mAX_DPH_SUM], j <- [0..i-1]]

      sum_tcs		<- mapM (externalTyCon dph_Repr)
				(numbered "Sum" 2 mAX_DPH_SUM)

      let selTys        = listArray (2, mAX_DPH_SUM) sel_tys
          selReplicates = listArray (2, mAX_DPH_SUM) sel_replicates
          selPicks      = listArray (2, mAX_DPH_SUM) sel_picks
          selTagss      = listArray (2, mAX_DPH_SUM) sel_tags
          selEls        = array     ((2,0), (mAX_DPH_SUM, mAX_DPH_SUM)) sel_els
          sumTyCons     = listArray (2, mAX_DPH_SUM) sum_tcs


      voidVar          <- externalVar dph_Repr		(fsLit "void")
      pvoidVar         <- externalVar dph_Repr		(fsLit "pvoid")
      fromVoidVar      <- externalVar dph_Repr		(fsLit "fromVoid")
      punitVar         <- externalVar dph_Repr		(fsLit "punit")
      closureVar       <- externalVar dph_Closure	(fsLit "closure")
      applyVar         <- externalVar dph_Closure	(fsLit "$:")
      liftedClosureVar <- externalVar dph_Closure	(fsLit "liftedClosure")
      liftedApplyVar   <- externalVar dph_Closure	(fsLit "liftedApply")
      replicatePDVar   <- externalVar dph_PArray	(fsLit "replicatePD")
      emptyPDVar       <- externalVar dph_PArray	(fsLit "emptyPD")
      packByTagPDVar   <- externalVar dph_PArray	(fsLit "packByTagPD")

      combines 		<- mapM (externalVar dph_PArray)
                       		[mkFastString ("combine" ++ show i ++ "PD")
                          		| i <- [2..mAX_DPH_COMBINE]]
      let combinePDVars = listArray (2, mAX_DPH_COMBINE) combines

      scalarClass 	<- externalClass dph_PArray	(fsLit "Scalar")
      scalar_map	<- externalVar	dph_Scalar	(fsLit "scalar_map")
      scalar_zip2	<- externalVar	dph_Scalar	(fsLit "scalar_zipWith")
      scalar_zips	<- mapM (externalVar dph_Scalar)
                          	(numbered "scalar_zipWith" 3 mAX_DPH_SCALAR_ARGS)

      let scalarZips 	= listArray (1, mAX_DPH_SCALAR_ARGS)
                                 (scalar_map : scalar_zip2 : scalar_zips)

      closures 		<- mapM (externalVar dph_Closure)
                       		(numbered "closure" 1 mAX_DPH_SCALAR_ARGS)

      let closureCtrFuns = listArray (1, mAX_DPH_COMBINE) closures

      liftingContext	<- liftM (\u -> mkSysLocal (fsLit "lc") u intPrimTy)
				newUnique

      return   $ Builtins 
               { dphModules       = mods
               , parrayTyCon      = parrayTyCon
               , parrayDataCon    = parrayDataCon
               , pdataTyCon       = pdataTyCon
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , paPRSel          = paPRSel
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
    mods@(Modules {
               dph_PArray         = dph_PArray
             , dph_Repr           = dph_Repr
             , dph_Closure        = dph_Closure
             , dph_Scalar         = dph_Scalar
             , dph_Unboxed        = dph_Unboxed
             })
      = dph_Modules pkg

    load get_mod = dsLoadModule doc mod
      where
        mod = get_mod mods 
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

  where	defaultTyCons :: DsM [TyCon]
	defaultTyCons
  	 = do	word8 <- dsLookupTyCon word8TyConName
		return [intTyCon, boolTyCon, doubleTyCon, word8]


-- | Get a list of names to `DataCon`s in the mock prelude.
initBuiltinDataCons :: Builtins -> [(Name, DataCon)]
initBuiltinDataCons _
  = [(dataConName dc, dc)| dc <- defaultDataCons]
  where	defaultDataCons :: [DataCon]
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
initBuiltinBoxedTyCons 
  = return . builtinBoxedTyCons
  where	builtinBoxedTyCons :: Builtins -> [(Name, TyCon)]
	builtinBoxedTyCons _ 
  		= [(tyConName intPrimTyCon, intTyCon)]


-- | Get a list of all scalar functions in the mock prelude.
initBuiltinScalars :: Builtins -> DsM [Var]
initBuiltinScalars bi
  = mapM (uncurry externalVar) (preludeScalars $ dphModules bi)


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



-- Set up the data structures provided by 'Vectorise.Builtins'.

module Vectorise.Builtins.Initialise (
  -- * Initialisation
  initBuiltins, initBuiltinVars, initBuiltinTyCons,
  initBuiltinPAs, initBuiltinPRs
) where

import Vectorise.Builtins.Base
import Vectorise.Builtins.Modules

import BasicTypes
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
import Id
import FastString
import Outputable

import Control.Monad
import Data.Array

-- |Create the initial map of builtin types and functions.
--
initBuiltins :: PackageId  -- ^ package id the builtins are in, eg dph-common
             -> DsM Builtins
initBuiltins pkg
 = do mapM_ load dph_Orphans

      -- From dph-common:Data.Array.Parallel.PArray.PData
      --     PData is a type family that maps an element type onto the type
      --     we use to hold an array of those elements.
      pdataTyCon  <- externalTyCon  dph_PArray_PData  (fsLit "PData")

      --     PR is a type class that holds the primitive operators we can 
      --     apply to array data. Its functions take arrays in terms of PData types.
      prClass           <- externalClass        dph_PArray_PData  (fsLit "PR")
      let prTyCon     = classTyCon prClass
          [prDataCon] = tyConDataCons prTyCon


      -- From dph-common:Data.Array.Parallel.PArray.PRepr
      preprTyCon  <- externalTyCon  dph_PArray_PRepr  (fsLit "PRepr")
      paClass           <- externalClass        dph_PArray_PRepr  (fsLit "PA")
      let paTyCon     = classTyCon paClass
          [paDataCon] = tyConDataCons paTyCon
          paPRSel     = classSCSelId paClass 0

      replicatePDVar    <- externalVar          dph_PArray_PRepr  (fsLit "replicatePD")
      emptyPDVar        <- externalVar          dph_PArray_PRepr  (fsLit "emptyPD")
      packByTagPDVar    <- externalVar          dph_PArray_PRepr  (fsLit "packByTagPD")
      combines    <- mapM (externalVar dph_PArray_PRepr)
                          [mkFastString ("combine" ++ show i ++ "PD")
                              | i <- [2..mAX_DPH_COMBINE]]

      let combinePDVars = listArray (2, mAX_DPH_COMBINE) combines


      -- From dph-common:Data.Array.Parallel.PArray.Scalar
      --     Scalar is the class of scalar values. 
      --     The dictionary contains functions to coerce U.Arrays of scalars
      --     to and from the PData representation.
      scalarClass   <- externalClass        dph_PArray_Scalar (fsLit "Scalar")


      -- From dph-common:Data.Array.Parallel.Lifted.PArray
      --   A PArray (Parallel Array) holds the array length and some array elements
      --   represented by the PData type family.
      parrayTyCon <- externalTyCon  dph_PArray_Base   (fsLit "PArray")
      let [parrayDataCon] = tyConDataCons parrayTyCon

      -- From dph-common:Data.Array.Parallel.PArray.Types
      voidTyCon   <- externalTyCon        dph_PArray_Types  (fsLit "Void")
      voidVar     <- externalVar          dph_PArray_Types  (fsLit "void")
      fromVoidVar <- externalVar          dph_PArray_Types  (fsLit "fromVoid")
      wrapTyCon   <- externalTyCon        dph_PArray_Types  (fsLit "Wrap")
      sum_tcs     <- mapM (externalTyCon  dph_PArray_Types) (numbered "Sum" 2 mAX_DPH_SUM)

      -- from dph-common:Data.Array.Parallel.PArray.PDataInstances
      pvoidVar          <- externalVar dph_PArray_PDataInstances  (fsLit "pvoid")
      punitVar          <- externalVar dph_PArray_PDataInstances  (fsLit "punit")


      closureTyCon  <- externalTyCon dph_Closure     (fsLit ":->")


      -- From dph-common:Data.Array.Parallel.Lifted.Unboxed
      sel_tys   <- mapM (externalType dph_Unboxed)
                            (numbered "Sel" 2 mAX_DPH_SUM)

      sel_replicates  <- mapM (externalFun dph_Unboxed)
        (numbered_hash "replicateSel" 2 mAX_DPH_SUM)

      sel_picks   <- mapM (externalFun dph_Unboxed)
        (numbered_hash "pickSel" 2 mAX_DPH_SUM)

      sel_tags    <- mapM (externalFun dph_Unboxed)
        (numbered "tagsSel" 2 mAX_DPH_SUM)

      sel_els   <- mapM mk_elements
        [(i,j) | i <- [2..mAX_DPH_SUM], j <- [0..i-1]]


      let selTys        = listArray (2, mAX_DPH_SUM) sel_tys
          selReplicates = listArray (2, mAX_DPH_SUM) sel_replicates
          selPicks      = listArray (2, mAX_DPH_SUM) sel_picks
          selTagss      = listArray (2, mAX_DPH_SUM) sel_tags
          selEls        = array     ((2,0), (mAX_DPH_SUM, mAX_DPH_SUM)) sel_els
          sumTyCons     = listArray (2, mAX_DPH_SUM) sum_tcs



      closureVar       <- externalVar dph_Closure (fsLit "closure")
      applyVar         <- externalVar dph_Closure (fsLit "$:")
      liftedClosureVar <- externalVar dph_Closure (fsLit "liftedClosure")
      liftedApplyVar   <- externalVar dph_Closure (fsLit "liftedApply")

      scalar_map  <- externalVar  dph_Scalar  (fsLit "scalar_map")
      scalar_zip2   <- externalVar  dph_Scalar  (fsLit "scalar_zipWith")
      scalar_zips <- mapM (externalVar dph_Scalar)
                            (numbered "scalar_zipWith" 3 mAX_DPH_SCALAR_ARGS)

      let scalarZips  = listArray (1, mAX_DPH_SCALAR_ARGS)
                                 (scalar_map : scalar_zip2 : scalar_zips)

      closures    <- mapM (externalVar dph_Closure)
                          (numbered "closure" 1 mAX_DPH_SCALAR_ARGS)

      let closureCtrFuns = listArray (1, mAX_DPH_COMBINE) closures

      liftingContext  <- liftM (\u -> mkSysLocal (fsLit "lc") u intPrimTy)
        newUnique

      return   $ Builtins 
               { dphModules       = mods
               , parrayTyCon      = parrayTyCon
               , parrayDataCon    = parrayDataCon
               , pdataTyCon       = pdataTyCon
               , paClass          = paClass
               , paTyCon          = paTyCon
               , paDataCon        = paDataCon
               , paPRSel          = paPRSel
               , preprTyCon       = preprTyCon
               , prClass          = prClass
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
    -- Extract out all the modules we'll use.
    -- These are the modules from the DPH base library that contain
    --  the primitive array types and functions that vectorised code uses.
    mods@(Modules 
                { dph_PArray_Base               = dph_PArray_Base
                , dph_PArray_Scalar             = dph_PArray_Scalar
                , dph_PArray_PRepr              = dph_PArray_PRepr
                , dph_PArray_PData              = dph_PArray_PData
                , dph_PArray_PDataInstances     = dph_PArray_PDataInstances
                , dph_PArray_Types              = dph_PArray_Types
                , dph_Closure                   = dph_Closure
                , dph_Scalar                    = dph_Scalar
                , dph_Unboxed                   = dph_Unboxed
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
--
initBuiltinVars :: Builtins -> DsM [(Var, Var)]
initBuiltinVars (Builtins { dphModules = mods })
  = do
      cvars <- zipWithM externalVar cmods cfs
      return $ zip (map dataConWorkId cons) cvars
  where
    (cons, cmods, cfs) = unzip3 (preludeDataCons mods)

    preludeDataCons :: Modules -> [(DataCon, Module, FastString)]
    preludeDataCons (Modules { dph_Prelude_Tuple = dph_Prelude_Tuple })
      = [mk_tup n dph_Prelude_Tuple (mkFastString $ "tup" ++ show n) | n <- [2..3]]
      where
        mk_tup n mod name = (tupleCon BoxedTuple n, mod, name)

-- |Get a list of names to `TyCon`s in the mock prelude.
--
initBuiltinTyCons :: Builtins -> DsM [(Name, TyCon)]
initBuiltinTyCons bi
  = do
      -- parr <- externalTyCon dph_Prelude_PArr (fsLit "PArr")
      return $ (tyConName funTyCon, closureTyCon bi)
             : (parrTyConName,      parrayTyCon bi)

             -- FIXME: temporary
             : (tyConName $ parrayTyCon bi, parrayTyCon bi)
             : []

-- |Get the names of all buildin instance functions for the PA class.
--
initBuiltinPAs :: Builtins -> (InstEnv, InstEnv) -> DsM [(Name, Var)]
initBuiltinPAs (Builtins { dphModules = mods }) insts
  = liftM (initBuiltinDicts insts) (externalClass (dph_PArray_PRepr mods) (fsLit "PA"))

-- |Get the names of all builtin instance functions for the PR class.
--
initBuiltinPRs :: Builtins -> (InstEnv, InstEnv) -> DsM [(Name, Var)]
initBuiltinPRs (Builtins { dphModules = mods }) insts
  = liftM (initBuiltinDicts insts) (externalClass (dph_PArray_PData mods) (fsLit "PR"))

-- |Get the names of all DPH instance functions for this class.
--
initBuiltinDicts :: (InstEnv, InstEnv) -> Class -> [(Name, Var)]
initBuiltinDicts insts cls = map find $ classInstances insts cls
  where
    find i | [Just tc] <- instanceRoughTcs i = (tc, instanceDFunId i)
           | otherwise                       = pprPanic "Invalid DPH instance" (ppr i)


-- Auxilliary look up functions ----------------

-- Lookup some variable given its name and the module that contains it.
--
externalVar :: Module -> FastString -> DsM Var
externalVar mod fs
  = dsLookupGlobalId =<< lookupOrig mod (mkVarOccFS fs)

-- Like `externalVar` but wrap the `Var` in a `CoreExpr`.
--
externalFun :: Module -> FastString -> DsM CoreExpr
externalFun mod fs
 = do var <- externalVar mod fs
      return $ Var var

-- Lookup some `TyCon` given its name and the module that contains it.
--
externalTyCon :: Module -> FastString -> DsM TyCon
externalTyCon mod fs
  = dsLookupTyCon =<< lookupOrig mod (mkTcOccFS fs)

-- Lookup some `Type` given its name and the module that contains it.
--
externalType :: Module -> FastString -> DsM Type
externalType mod fs
 = do  tycon <- externalTyCon mod fs
       return $ mkTyConApp tycon []

-- Lookup some `Class` given its name and the module that contains it.
--
externalClass :: Module -> FastString -> DsM Class
externalClass mod fs
  = fmap (maybe (panic "externalClass") id . tyConClass_maybe) $ dsLookupTyCon =<< lookupOrig mod (mkClsOccFS fs)

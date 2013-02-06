-- Set up the data structures provided by 'Vectorise.Builtins'.

module Vectorise.Builtins.Initialise (
  -- * Initialisation
  initBuiltins, initBuiltinVars
) where

import Vectorise.Builtins.Base

import BasicTypes
import TysPrim
import DsMonad
import TysWiredIn
import DataCon
import TyCon
import Class
import CoreSyn
import Type
import NameEnv
import Name
import Id
import FastString
import Outputable

import Control.Monad
import Data.Array


-- |Create the initial map of builtin types and functions.
--
initBuiltins :: DsM Builtins
initBuiltins
 = do {   -- 'PArray: representation type for parallel arrays
      ; parrayTyCon <- externalTyCon (fsLit "PArray")
      
          -- 'PData': type family mapping array element types to array representation types
          -- Not all backends use `PDatas`.
      ; pdataTyCon  <- externalTyCon (fsLit "PData")
      ; pdatasTyCon <- externalTyCon (fsLit "PDatas")

          -- 'PR': class of basic array operators operating on 'PData' types
      ; prClass     <- externalClass (fsLit "PR")
      ; let prTyCon     = classTyCon prClass

          -- 'PRepr': type family mapping element types to representation types
      ; preprTyCon  <- externalTyCon (fsLit "PRepr")

          -- 'PA': class of basic operations on arrays (parametrised by the element type)
      ; paClass     <- externalClass (fsLit "PA")
      ; let paTyCon     = classTyCon paClass
            [paDataCon] = tyConDataCons paTyCon
            paPRSel     = classSCSelId paClass 0

          -- Functions on array representations
      ; replicatePDVar <- externalVar (fsLit "replicatePD")
      ; replicate_vars <- mapM externalVar (suffixed "replicatePA" aLL_DPH_PRIM_TYCONS)
      ; emptyPDVar     <- externalVar (fsLit "emptyPD")
      ; empty_vars     <- mapM externalVar (suffixed "emptyPA" aLL_DPH_PRIM_TYCONS)
      ; packByTagPDVar <- externalVar (fsLit "packByTagPD")
      ; packByTag_vars <- mapM externalVar (suffixed "packByTagPA" aLL_DPH_PRIM_TYCONS)
      ; let combineNamesD = [("combine" ++ show i ++ "PD") | i <- [2..mAX_DPH_COMBINE]]
      ; let combineNamesA = [("combine" ++ show i ++ "PA") | i <- [2..mAX_DPH_COMBINE]]
      ; combines       <- mapM externalVar (map mkFastString combineNamesD)
      ; combines_vars  <- mapM (mapM externalVar) $
                            map (\name -> suffixed name aLL_DPH_PRIM_TYCONS) combineNamesA
      ; let replicatePD_PrimVars = mkNameEnv (zip aLL_DPH_PRIM_TYCONS replicate_vars)
            emptyPD_PrimVars     = mkNameEnv (zip aLL_DPH_PRIM_TYCONS empty_vars)
            packByTagPD_PrimVars = mkNameEnv (zip aLL_DPH_PRIM_TYCONS packByTag_vars)
            combinePDVars        = listArray (2, mAX_DPH_COMBINE) combines
            combinePD_PrimVarss  = listArray (2, mAX_DPH_COMBINE)
                                     [ mkNameEnv (zip aLL_DPH_PRIM_TYCONS vars)
                                     | vars <- combines_vars]

          -- 'Scalar': class moving between plain unboxed arrays and 'PData' representations
      ; scalarClass <- externalClass (fsLit "Scalar")

          -- N-ary maps ('zipWith' family)
      ; scalar_map       <- externalVar (fsLit "scalar_map")
      ; scalar_zip2      <- externalVar (fsLit "scalar_zipWith")
      ; scalar_zips      <- mapM externalVar (numbered "scalar_zipWith" 3 mAX_DPH_SCALAR_ARGS)
      ; let scalarZips   = listArray (1, mAX_DPH_SCALAR_ARGS) 
                                     (scalar_map : scalar_zip2 : scalar_zips)

          -- Types and functions for generic type representations
      ; voidTyCon        <- externalTyCon (fsLit "Void")
      ; voidVar          <- externalVar   (fsLit "void")
      ; fromVoidVar      <- externalVar   (fsLit "fromVoid")
      ; sum_tcs          <- mapM externalTyCon (numbered "Sum" 2 mAX_DPH_SUM)
      ; let sumTyCons    = listArray (2, mAX_DPH_SUM) sum_tcs
      ; wrapTyCon        <- externalTyCon (fsLit "Wrap")
      ; pvoidVar         <- externalVar   (fsLit "pvoid")
      ; pvoidsVar        <- externalVar   (fsLit "pvoids#")

          -- Types and functions for closure conversion
      ; closureTyCon     <- externalTyCon (fsLit ":->")
      ; closureVar       <- externalVar   (fsLit "closure")
      ; liftedClosureVar <- externalVar   (fsLit "liftedClosure")
      ; applyVar         <- externalVar   (fsLit "$:")
      ; liftedApplyVar   <- externalVar   (fsLit "liftedApply")
      ; closures         <- mapM externalVar (numbered "closure" 1 mAX_DPH_SCALAR_ARGS)
      ; let closureCtrFuns = listArray (1, mAX_DPH_SCALAR_ARGS) closures

          -- Types and functions for selectors
      ; sel_tys          <- mapM externalType (numbered "Sel"  2 mAX_DPH_SUM)
      ; sels_tys         <- mapM externalType (numbered "Sels" 2 mAX_DPH_SUM)
      ; sels_length      <- mapM externalFun  (numbered_hash "lengthSels"   2 mAX_DPH_SUM)
      ; sel_replicates   <- mapM externalFun  (numbered_hash "replicateSel" 2 mAX_DPH_SUM)
      ; sel_tags         <- mapM externalFun  (numbered "tagsSel" 2 mAX_DPH_SUM)
      ; sel_elements     <- mapM mk_elements [(i,j) | i <- [2..mAX_DPH_SUM], j <- [0..i-1]]
      ; let selTys        = listArray (2, mAX_DPH_SUM) sel_tys
            selsTys       = listArray (2, mAX_DPH_SUM) sels_tys
            selsLengths   = listArray (2, mAX_DPH_SUM) sels_length
            selReplicates = listArray (2, mAX_DPH_SUM) sel_replicates
            selTagss      = listArray (2, mAX_DPH_SUM) sel_tags
            selElementss  = array     ((2, 0), (mAX_DPH_SUM, mAX_DPH_SUM)) sel_elements

          -- Distinct local variable
      ; liftingContext  <- liftM (\u -> mkSysLocal (fsLit "lc") u intPrimTy) newUnique

      ; return $ Builtins 
               { parrayTyCon          = parrayTyCon
               , pdataTyCon           = pdataTyCon
               , pdatasTyCon          = pdatasTyCon
               , preprTyCon           = preprTyCon
               , prClass              = prClass
               , prTyCon              = prTyCon
               , paClass              = paClass
               , paTyCon              = paTyCon
               , paDataCon            = paDataCon
               , paPRSel              = paPRSel
               , replicatePDVar       = replicatePDVar
               , replicatePD_PrimVars = replicatePD_PrimVars
               , emptyPDVar           = emptyPDVar
               , emptyPD_PrimVars     = emptyPD_PrimVars
               , packByTagPDVar       = packByTagPDVar
               , packByTagPD_PrimVars = packByTagPD_PrimVars
               , combinePDVars        = combinePDVars
               , combinePD_PrimVarss  = combinePD_PrimVarss
               , scalarClass          = scalarClass
               , scalarZips           = scalarZips
               , voidTyCon            = voidTyCon
               , voidVar              = voidVar
               , fromVoidVar          = fromVoidVar
               , sumTyCons            = sumTyCons
               , wrapTyCon            = wrapTyCon
               , pvoidVar             = pvoidVar
               , pvoidsVar            = pvoidsVar
               , closureTyCon         = closureTyCon
               , closureVar           = closureVar
               , liftedClosureVar     = liftedClosureVar
               , applyVar             = applyVar
               , liftedApplyVar       = liftedApplyVar
               , closureCtrFuns       = closureCtrFuns
               , selTys               = selTys
               , selsTys              = selsTys
               , selsLengths          = selsLengths
               , selReplicates        = selReplicates
               , selTagss             = selTagss
               , selElementss         = selElementss
               , liftingContext       = liftingContext
               }
      }
  where
    suffixed :: String -> [Name] -> [FastString]
    suffixed pfx ns = [mkFastString (pfx ++ "_" ++ (occNameString . nameOccName) n) | n <- ns]

    -- Make a list of numbered strings in some range, eg foo3, foo4, foo5
    numbered :: String -> Int -> Int -> [FastString]
    numbered pfx m n = [mkFastString (pfx ++ show i) | i <- [m..n]]

    numbered_hash :: String -> Int -> Int -> [FastString]
    numbered_hash pfx m n = [mkFastString (pfx ++ show i ++ "#") | i <- [m..n]]

    mk_elements :: (Int, Int) -> DsM ((Int, Int), CoreExpr)
    mk_elements (i,j)
      = do { v <- externalVar $ mkFastString ("elementsSel" ++ show i ++ "_" ++ show j ++ "#")
           ; return ((i, j), Var v)
           }

-- |Get the mapping of names in the Prelude to names in the DPH library.
--
initBuiltinVars :: Builtins -> DsM [(Var, Var)]
-- FIXME: must be replaced by VECTORISE pragmas!!!
initBuiltinVars (Builtins { })
  = do
      cvars <- mapM externalVar cfs
      return $ zip (map dataConWorkId cons) cvars
  where
    (cons, cfs) = unzip preludeDataCons

    preludeDataCons :: [(DataCon, FastString)]
    preludeDataCons
      = [mk_tup n (mkFastString $ "tup" ++ show n) | n <- [2..5]]
      where
        mk_tup n name = (tupleCon BoxedTuple n, name)


-- Auxilliary look up functions -----------------------------------------------

-- |Lookup a variable given its name and the module that contains it.
externalVar :: FastString -> DsM Var
externalVar fs = dsLookupDPHRdrEnv (mkVarOccFS fs) >>= dsLookupGlobalId


-- |Like `externalVar` but wrap the `Var` in a `CoreExpr`.
externalFun :: FastString -> DsM CoreExpr
externalFun fs = Var <$> externalVar fs


-- |Lookup a 'TyCon' in 'Data.Array.Parallel.Prim', given its name.
--  Panic if there isn't one.
externalTyCon :: FastString -> DsM TyCon
externalTyCon fs = dsLookupDPHRdrEnv (mkTcOccFS fs) >>= dsLookupTyCon


-- |Lookup some `Type` in 'Data.Array.Parallel.Prim', given its name.
externalType :: FastString -> DsM Type
externalType fs
 = do  tycon <- externalTyCon fs
       return $ mkTyConApp tycon []


-- |Lookup a 'Class' in 'Data.Array.Parallel.Prim', given its name.
externalClass :: FastString -> DsM Class
externalClass fs 
  = do { tycon <- dsLookupDPHRdrEnv (mkClsOccFS fs) >>= dsLookupTyCon
       ; case tyConClass_maybe tycon of
           Nothing  -> pprPanic "Vectorise.Builtins.Initialise" $ 
                         ptext (sLit "Data.Array.Parallel.Prim.") <> 
                         ftext fs <+> ptext (sLit "is not a type class")
           Just cls -> return cls
       }

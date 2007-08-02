module VectType ( vectTyCon, vectType, vectTypeEnv,
                   PAInstance, painstInstance, buildPADict )
where

#include "HsVersions.h"

import VectMonad
import VectUtils

import HscTypes          ( TypeEnv, extendTypeEnvList, typeEnvTyCons )
import CoreSyn
import CoreUtils
import DataCon
import TyCon
import Type
import TypeRep
import Coercion
import FamInstEnv        ( FamInst, mkLocalFamInst )
import InstEnv           ( Instance, mkLocalInstance, instanceDFunId )
import OccName
import MkId
import BasicTypes        ( StrictnessMark(..), OverlapFlag(..), boolToRecFlag )
import Var               ( Var )
import Id                ( mkWildId )
import Name              ( Name, getOccName )
import NameEnv
import TysWiredIn        ( intTy, intDataCon )
import TysPrim           ( intPrimTy )

import Unique
import UniqFM
import UniqSet
import Digraph           ( SCC(..), stronglyConnComp )

import Outputable

import Control.Monad  ( liftM, liftM2, zipWithM, zipWithM_ )
import Data.List      ( inits, tails )

-- ----------------------------------------------------------------------------
-- Types

vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise = do
                  r <- lookupTyCon tc
                  case r of
                    Just tc' -> return tc'

                    -- FIXME: just for now
                    Nothing  -> pprTrace "ccTyCon:" (ppr tc) $ return tc

vectType :: Type -> VM Type
vectType ty | Just ty' <- coreView ty = vectType ty'
vectType (TyVarTy tv) = return $ TyVarTy tv
vectType (AppTy ty1 ty2) = liftM2 AppTy (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) = liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   = liftM2 TyConApp (builtin closureTyCon)
                                             (mapM vectType [ty1,ty2])
vectType ty@(ForAllTy _ _)
  = do
      mdicts   <- mapM paDictArgType tyvars
      mono_ty' <- vectType mono_ty
      return $ tyvars `mkForAllTys` ([dict | Just dict <- mdicts] `mkFunTys` mono_ty')
  where
    (tyvars, mono_ty) = splitForAllTys ty

vectType ty = pprPanic "vectType:" (ppr ty)

-- ----------------------------------------------------------------------------
-- Type definitions

type TyConGroup = ([TyCon], UniqSet TyCon)

data PAInstance = PAInstance {
                    painstInstance  :: Instance
                  , painstOrigTyCon :: TyCon
                  , painstVectTyCon :: TyCon
                  , painstArrTyCon  :: TyCon
                  }

vectTypeEnv :: TypeEnv -> VM (TypeEnv, [FamInst], [PAInstance])
vectTypeEnv env
  = do
      cs <- readGEnv $ mk_map . global_tycons
      let (conv_tcs, keep_tcs) = classifyTyCons cs groups
          keep_dcs             = concatMap tyConDataCons keep_tcs
      zipWithM_ defTyCon   keep_tcs keep_tcs
      zipWithM_ defDataCon keep_dcs keep_dcs
      new_tcs <- vectTyConDecls conv_tcs

      let orig_tcs = keep_tcs ++ conv_tcs
          vect_tcs  = keep_tcs ++ new_tcs

      parr_tcs <- zipWithM buildPArrayTyCon orig_tcs vect_tcs
      pa_insts <- sequence $ zipWith3 buildPAInstance orig_tcs vect_tcs parr_tcs
      
      let all_new_tcs = new_tcs ++ parr_tcs

      let new_env = extendTypeEnvList env
                       (map ATyCon all_new_tcs
                        ++ [ADataCon dc | tc <- all_new_tcs
                                        , dc <- tyConDataCons tc])

      return (new_env, map mkLocalFamInst parr_tcs, pa_insts)
  where
    tycons = typeEnvTyCons env
    groups = tyConGroups tycons

    mk_map env = listToUFM_Directly [(u, getUnique n /= u) | (u,n) <- nameEnvUniqueElts env]

    keep_tc tc = let dcs = tyConDataCons tc
                 in
                 defTyCon tc tc >> zipWithM_ defDataCon dcs dcs


vectTyConDecls :: [TyCon] -> VM [TyCon]
vectTyConDecls tcs = fixV $ \tcs' ->
  do
    mapM_ (uncurry defTyCon) (lazy_zip tcs tcs')
    mapM vectTyConDecl tcs
  where
    lazy_zip [] _ = []
    lazy_zip (x:xs) ~(y:ys) = (x,y) : lazy_zip xs ys

vectTyConDecl :: TyCon -> VM TyCon
vectTyConDecl tc
  = do
      name' <- cloneName mkVectTyConOcc name
      rhs'  <- vectAlgTyConRhs (algTyConRhs tc)

      return $ mkAlgTyCon name'
                          kind
                          tyvars
                          []              -- no stupid theta
                          rhs'
                          []              -- no selector ids
                          NoParentTyCon   -- FIXME
                          rec_flag        -- FIXME: is this ok?
                          False           -- FIXME: no generics
                          False           -- not GADT syntax
  where
    name   = tyConName tc
    kind   = tyConKind tc
    tyvars = tyConTyVars tc
    rec_flag = boolToRecFlag (isRecursiveTyCon tc)

vectAlgTyConRhs :: AlgTyConRhs -> VM AlgTyConRhs
vectAlgTyConRhs (DataTyCon { data_cons = data_cons
                           , is_enum   = is_enum
                           })
  = do
      data_cons' <- mapM vectDataCon data_cons
      zipWithM_ defDataCon data_cons data_cons'
      return $ DataTyCon { data_cons = data_cons'
                         , is_enum   = is_enum
                         }

vectDataCon :: DataCon -> VM DataCon
vectDataCon dc
  | not . null $ dataConExTyVars dc = pprPanic "vectDataCon: existentials" (ppr dc)
  | not . null $ dataConEqSpec   dc = pprPanic "vectDataCon: eq spec" (ppr dc)
  | otherwise
  = do
      name'    <- cloneName mkVectDataConOcc name
      tycon'   <- vectTyCon tycon
      arg_tys  <- mapM vectType rep_arg_tys
      wrk_name <- cloneName mkDataConWorkerOcc name'

      let ids      = mkDataConIds (panic "vectDataCon: wrapper id")
                                  wrk_name
                                  data_con
          data_con = mkDataCon name'
                               False           -- not infix
                               (map (const NotMarkedStrict) arg_tys)
                               []              -- no labelled fields
                               univ_tvs
                               []              -- no existential tvs for now
                               []              -- no eq spec for now
                               []              -- no theta
                               arg_tys
                               tycon'
                               []              -- no stupid theta
                               ids
      return data_con
  where
    name        = dataConName dc
    univ_tvs    = dataConUnivTyVars dc
    rep_arg_tys = dataConRepArgTys dc
    tycon       = dataConTyCon dc

data Shape = Shape {
               shapeReprTys    :: [Type]
             , shapeStrictness :: [StrictnessMark]
             , shapeLength     :: [CoreExpr] -> VM CoreExpr
             , shapeReplicate  :: CoreExpr -> CoreExpr -> VM [CoreExpr]
             }

tyConShape :: TyCon -> VM Shape
tyConShape vect_tc
  | isProductTyCon vect_tc
  = return $ Shape {
                shapeReprTys    = [intPrimTy]
              , shapeStrictness = [NotMarkedStrict]
              , shapeLength     = \[len] -> return len
              , shapeReplicate  = \len _ -> return [len]
              }

  | otherwise
  = do
      repr_ty <- mkPArrayType intTy   -- FIXME: we want to unbox this
      return $ Shape {
                 shapeReprTys    = [repr_ty]
               , shapeStrictness = [MarkedStrict]
               , shapeLength     = \[sel] -> lengthPA sel
               , shapeReplicate  = \len n -> do
                                               e <- replicatePA len n
                                               return [e]
               }
        
buildPArrayTyCon :: TyCon -> TyCon -> VM TyCon
buildPArrayTyCon orig_tc vect_tc = fixV $ \repr_tc ->
  do
    name'  <- cloneName mkPArrayTyConOcc orig_name
    parent <- buildPArrayParentInfo orig_name vect_tc repr_tc
    rhs    <- buildPArrayTyConRhs orig_name vect_tc repr_tc

    return $ mkAlgTyCon name'
                        kind
                        tyvars
                        []              -- no stupid theta
                        rhs
                        []              -- no selector ids
                        parent
                        rec_flag        -- FIXME: is this ok?
                        False           -- FIXME: no generics
                        False           -- not GADT syntax
  where
    orig_name = tyConName orig_tc
    name   = tyConName vect_tc
    kind   = tyConKind vect_tc
    tyvars = tyConTyVars vect_tc
    rec_flag = boolToRecFlag (isRecursiveTyCon vect_tc)
    

buildPArrayParentInfo :: Name -> TyCon -> TyCon -> VM TyConParent
buildPArrayParentInfo orig_name vect_tc repr_tc
  = do
      parray_tc <- builtin parrayTyCon
      co_name <- cloneName mkInstTyCoOcc (tyConName repr_tc)

      let inst_tys = [mkTyConApp vect_tc (map mkTyVarTy tyvars)]

      return . FamilyTyCon parray_tc inst_tys
             $ mkFamInstCoercion co_name
                                 tyvars
                                 parray_tc
                                 inst_tys
                                 repr_tc
  where
    tyvars = tyConTyVars vect_tc

buildPArrayTyConRhs :: Name -> TyCon -> TyCon -> VM AlgTyConRhs
buildPArrayTyConRhs orig_name vect_tc repr_tc
  = do
      data_con <- buildPArrayDataCon orig_name vect_tc repr_tc
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPArrayDataCon :: Name -> TyCon -> TyCon -> VM DataCon
buildPArrayDataCon orig_name vect_tc repr_tc
  = do
      dc_name  <- cloneName mkPArrayDataConOcc orig_name
      shape    <- tyConShape vect_tc
      repr_tys <- mapM mkPArrayType types
      wrk_name <- cloneName mkDataConWorkerOcc  dc_name
      wrp_name <- cloneName mkDataConWrapperOcc dc_name

      let ids      = mkDataConIds wrp_name wrk_name data_con
          data_con = mkDataCon dc_name
                               False
                               (shapeStrictness shape ++ map (const NotMarkedStrict) repr_tys)
                               []
                               (tyConTyVars vect_tc)
                               []
                               []
                               []
                               (shapeReprTys shape ++ repr_tys)
                               repr_tc
                               []
                               ids

      return data_con
  where
    types = [ty | dc <- tyConDataCons vect_tc
                , ty <- dataConRepArgTys dc]

buildPAInstance :: TyCon -> TyCon -> TyCon -> VM PAInstance
buildPAInstance orig_tc vect_tc arr_tc
  = do
      pa <- builtin paClass
      let inst_ty = mkForAllTys tvs
                  . (mkFunTys $ mkPredTys [ClassP pa [ty] | ty <- arg_tys])
                  $ mkPredTy (ClassP pa [mkTyConApp vect_tc arg_tys])

      dfun <- newExportedVar (mkPADFunOcc $ getOccName vect_tc) inst_ty

      return $ PAInstance {
                 painstInstance  = mkLocalInstance dfun NoOverlap
               , painstOrigTyCon = orig_tc
               , painstVectTyCon = vect_tc
               , painstArrTyCon  = arr_tc
               }
  where
    tvs = tyConTyVars arr_tc
    arg_tys = mkTyVarTys tvs

buildPADict :: PAInstance -> VM [(Var, CoreExpr)]
buildPADict (PAInstance {
               painstInstance  = inst
             , painstVectTyCon = vect_tc
             , painstArrTyCon  = arr_tc })
  = polyAbstract (tyConTyVars arr_tc) $ \abstract ->
    do
      shape <- tyConShape vect_tc
      meth_binds <- mapM (mk_method shape) paMethods
      let meth_exprs = map (Var . fst) meth_binds

      pa_dc <- builtin paDictDataCon
      let dict = mkConApp pa_dc (Type (mkTyConApp vect_tc arg_tys) : meth_exprs)
          body = Let (Rec meth_binds) dict
      return [(instanceDFunId inst, mkInlineMe $ abstract body)]
  where
    tvs = tyConTyVars arr_tc
    arg_tys = mkTyVarTys tvs

    mk_method shape (name, build)
      = localV
      $ do
          body <- build shape vect_tc arr_tc
          var  <- newLocalVar name (exprType body)
          return (var, mkInlineMe body)
          
paMethods = [(FSLIT("lengthPA"),    buildLengthPA),
             (FSLIT("replicatePA"), buildReplicatePA)]

buildLengthPA :: Shape -> TyCon -> TyCon -> VM CoreExpr
buildLengthPA shape vect_tc arr_tc
  = do
      parr_ty <- mkPArrayType (mkTyConApp vect_tc arg_tys)
      arg    <- newLocalVar FSLIT("xs") parr_ty
      shapes <- mapM (newLocalVar FSLIT("sh")) shape_tys
      wilds  <- mapM newDummyVar repr_tys
      let scrut    = unwrapFamInstScrut arr_tc arg_tys (Var arg)
          scrut_ty = exprType scrut

      body <- shapeLength shape (map Var shapes)

      return . Lam arg
             $ Case scrut (mkWildId scrut_ty) intPrimTy
                    [(DataAlt repr_dc, shapes ++ wilds, body)]
  where
    arg_tys = mkTyVarTys $ tyConTyVars arr_tc
    [repr_dc] = tyConDataCons arr_tc
    
    shape_tys = shapeReprTys shape
    repr_tys  = drop (length shape_tys) (dataConRepArgTys repr_dc)

-- data T = C0 t1 ... tm
--          ...
--          Ck u1 ... un
--
-- data [:T:] = A ![:Int:] [:t1:] ... [:un:]
--
-- replicatePA :: Int# -> T -> [:T:]
-- replicatePA n# t
--   = let c = case t of
--               C0 _ ... _ -> 0
--               ...
--               Ck _ ... _ -> k
--
--         xs1 = case t of
--                 C0 x1 _ ... _ -> replicatePA @t1 n# x1
--                 _             -> emptyPA @t1
--
--         ...
--
--         ysn = case t of
--                 Ck _ ... _ yn -> replicatePA @un n# yn
--                 _             -> emptyPA @un
--     in
--     A (replicatePA @Int n# c) xs1 ... ysn
--
--

buildReplicatePA :: Shape -> TyCon -> TyCon -> VM CoreExpr
buildReplicatePA shape vect_tc arr_tc
  = do
      len_var <- newLocalVar FSLIT("n") intPrimTy
      val_var <- newLocalVar FSLIT("x") val_ty

      let len = Var len_var
          val = Var val_var

      shape_reprs <- shapeReplicate shape len (ctr_num val)
      reprs <- liftM concat $ mapM (mk_comp_arrs len val) vect_dcs

      return . mkLams [len_var, val_var]
             . wrapFamInstBody arr_tc arg_tys
             $ mkConApp arr_dc (map Type arg_tys ++ shape_reprs ++ reprs)
  where
    arg_tys = mkTyVarTys (tyConTyVars arr_tc)
    val_ty  = mkTyConApp vect_tc arg_tys
    wild    = mkWildId val_ty
    vect_dcs = tyConDataCons vect_tc
    [arr_dc] = tyConDataCons arr_tc

    ctr_num val = Case val wild intTy (zipWith ctr_num_alt vect_dcs [0..])
    ctr_num_alt dc i = (DataAlt dc, map mkWildId (dataConRepArgTys dc),
                                    mkConApp intDataCon [mkIntLitInt i])


    mk_comp_arrs len val dc = let tys = dataConRepArgTys dc
                                  wilds = map mkWildId tys
                              in
                              sequence (zipWith3 (mk_comp_arr len val dc)
                                       tys (inits wilds) (tails wilds))

    mk_comp_arr len val dc ty pre (_:post)
      = do
          var   <- newLocalVar FSLIT("x") ty
          rep   <- replicatePA len (Var var)
          empty <- emptyPA ty
          arr_ty <- mkPArrayType ty

          return $ Case val wild arr_ty
                     [(DEFAULT, [], empty), (DataAlt dc, pre ++ (var : post), rep)]

-- | Split the given tycons into two sets depending on whether they have to be
-- converted (first list) or not (second list). The first argument contains
-- information about the conversion status of external tycons:
-- 
--   * tycons which have converted versions are mapped to True
--   * tycons which are not changed by vectorisation are mapped to False
--   * tycons which can't be converted are not elements of the map
--
classifyTyCons :: UniqFM Bool -> [TyConGroup] -> ([TyCon], [TyCon])
classifyTyCons = classify [] []
  where
    classify conv keep cs [] = (conv, keep)
    classify conv keep cs ((tcs, ds) : rs)
      | can_convert && must_convert
        = classify (tcs ++ conv) keep (cs `addListToUFM` [(tc,True) | tc <- tcs]) rs
      | can_convert
        = classify conv (tcs ++ keep) (cs `addListToUFM` [(tc,False) | tc <- tcs]) rs
      | otherwise
        = classify conv keep cs rs
      where
        refs = ds `delListFromUniqSet` tcs

        can_convert  = isNullUFM (refs `minusUFM` cs) && all convertable tcs
        must_convert = foldUFM (||) False (intersectUFM_C const cs refs)

        convertable tc = isDataTyCon tc && all isVanillaDataCon (tyConDataCons tc)
    
-- | Compute mutually recursive groups of tycons in topological order
--
tyConGroups :: [TyCon] -> [TyConGroup]
tyConGroups tcs = map mk_grp (stronglyConnComp edges)
  where
    edges = [((tc, ds), tc, uniqSetToList ds) | tc <- tcs
                                , let ds = tyConsOfTyCon tc]

    mk_grp (AcyclicSCC (tc, ds)) = ([tc], ds)
    mk_grp (CyclicSCC els)       = (tcs, unionManyUniqSets dss)
      where
        (tcs, dss) = unzip els

tyConsOfTyCon :: TyCon -> UniqSet TyCon
tyConsOfTyCon 
  = tyConsOfTypes . concatMap dataConRepArgTys . tyConDataCons

tyConsOfType :: Type -> UniqSet TyCon
tyConsOfType ty
  | Just ty' <- coreView ty    = tyConsOfType ty'
tyConsOfType (TyVarTy v)       = emptyUniqSet
tyConsOfType (TyConApp tc tys) = extend (tyConsOfTypes tys)
  where
    extend | isUnLiftedTyCon tc
           || isTupleTyCon   tc = id

           | otherwise          = (`addOneToUniqSet` tc)

tyConsOfType (AppTy a b)       = tyConsOfType a `unionUniqSets` tyConsOfType b
tyConsOfType (FunTy a b)       = (tyConsOfType a `unionUniqSets` tyConsOfType b)
                                 `addOneToUniqSet` funTyCon
tyConsOfType (ForAllTy _ ty)   = tyConsOfType ty
tyConsOfType other             = pprPanic "ClosureConv.tyConsOfType" $ ppr other

tyConsOfTypes :: [Type] -> UniqSet TyCon
tyConsOfTypes = unionManyUniqSets . map tyConsOfType


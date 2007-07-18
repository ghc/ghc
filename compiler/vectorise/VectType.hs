module VectType ( vectTyCon, vectType, vectTypeEnv )
where

#include "HsVersions.h"

import VectMonad
import VectUtils

import HscTypes          ( TypeEnv, extendTypeEnvList, typeEnvTyCons )
import DataCon
import TyCon
import Type
import TypeRep
import Coercion
import OccName
import MkId
import BasicTypes        ( StrictnessMark(..), boolToRecFlag )
import NameEnv
import TysWiredIn

import Unique
import UniqFM
import UniqSet
import Digraph           ( SCC(..), stronglyConnComp )

import Outputable

import Control.Monad  ( liftM2, zipWithM_ )

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

vectTypeEnv :: TypeEnv -> VM TypeEnv
vectTypeEnv env
  = do
      cs <- readGEnv $ mk_map . global_tycons
      let (conv_tcs, keep_tcs) = classifyTyCons cs groups
          keep_dcs             = concatMap tyConDataCons keep_tcs
      zipWithM_ defTyCon   keep_tcs keep_tcs
      zipWithM_ defDataCon keep_dcs keep_dcs
      vect_tcs <- vectTyConDecls conv_tcs
      parr_tcs <- mapM buildPArrayTyCon (keep_tcs ++ vect_tcs)
      let new_tcs = vect_tcs ++ parr_tcs
      return $ extendTypeEnvList env
                 (map ATyCon new_tcs ++ [ADataCon dc | tc <- new_tcs
                                                     , dc <- tyConDataCons tc])
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

buildPArrayTyCon :: TyCon -> VM TyCon
buildPArrayTyCon orig_tc = fixV $ \repr_tc ->
  do
    name'  <- cloneName mkPArrayTyConOcc name
    parent <- buildPArrayParentInfo orig_tc repr_tc
    rhs    <- buildPArrayTyConRhs orig_tc repr_tc

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
    name   = tyConName orig_tc
    kind   = tyConKind orig_tc
    tyvars = tyConTyVars orig_tc
    rec_flag = boolToRecFlag (isRecursiveTyCon orig_tc)
    

buildPArrayParentInfo :: TyCon -> TyCon -> VM TyConParent
buildPArrayParentInfo orig_tc repr_tc
  = do
      parray_tc <- builtin parrayTyCon
      co_name <- cloneName mkInstTyCoOcc (tyConName repr_tc)

      let inst_tys = [mkTyConApp orig_tc (map mkTyVarTy tyvars)]

      return . FamilyTyCon parray_tc inst_tys
             $ mkFamInstCoercion co_name
                                 tyvars
                                 parray_tc
                                 inst_tys
                                 repr_tc
  where
    tyvars = tyConTyVars orig_tc

buildPArrayTyConRhs :: TyCon -> TyCon -> VM AlgTyConRhs
buildPArrayTyConRhs orig_tc repr_tc
  = do
      data_con <- buildPArrayDataCon orig_tc repr_tc
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPArrayDataCon :: TyCon -> TyCon -> VM DataCon
buildPArrayDataCon orig_tc repr_tc
  = do
      name     <- cloneName mkPArrayDataConOcc (tyConName orig_tc)
      shape_ty <- mkPArrayType intTy   -- FIXME: we want to unbox this!
      repr_tys <- mapM mkPArrayType types
      wrk_name <- cloneName mkDataConWorkerOcc name
      wrp_name <- cloneName mkDataConWrapperOcc name

      let ids      = mkDataConIds wrp_name wrk_name data_con
          data_con = mkDataCon name
                               False
                               (MarkedStrict : map (const NotMarkedStrict) repr_tys)
                               []
                               (tyConTyVars orig_tc)
                               []
                               []
                               []
                               (shape_ty : repr_tys)
                               repr_tc
                               []
                               ids

      return data_con
  where
    types = [ty | dc <- tyConDataCons orig_tc
                , ty <- dataConRepArgTys dc]

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


module VectType ( vectTyCon, vectType, vectTypeEnv,
                   PAInstance, buildPADict )
where

#include "HsVersions.h"

import VectMonad
import VectUtils
import VectCore

import HscTypes          ( TypeEnv, extendTypeEnvList, typeEnvTyCons )
import CoreSyn
import CoreUtils
import BuildTyCl
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
import TysWiredIn        ( unitTy, unitTyCon, intTy, intDataCon, unitDataConId )
import TysPrim           ( intPrimTy )

import Unique
import UniqFM
import UniqSet
import Digraph           ( SCC(..), stronglyConnComp )

import Outputable

import Control.Monad  ( liftM, liftM2, zipWithM, zipWithM_ )
import Data.List      ( inits, tails, zipWith4, zipWith5 )

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
                    painstDFun      :: Var
                  , painstOrigTyCon :: TyCon
                  , painstVectTyCon :: TyCon
                  , painstArrTyCon  :: TyCon
                  }

vectTypeEnv :: TypeEnv -> VM (TypeEnv, [FamInst], [(Var, CoreExpr)])
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

      repr_tcs <- zipWithM buildPReprTyCon   orig_tcs vect_tcs
      parr_tcs <- zipWithM buildPArrayTyCon orig_tcs vect_tcs
      dfuns    <- mapM mkPADFun vect_tcs
      defTyConPAs (zip vect_tcs dfuns)
      binds    <- sequence (zipWith5 buildTyConBindings orig_tcs
                                                        vect_tcs
                                                        repr_tcs
                                                        parr_tcs
                                                        dfuns)

      let all_new_tcs = new_tcs ++ repr_tcs ++ parr_tcs

      let new_env = extendTypeEnvList env
                       (map ATyCon all_new_tcs
                        ++ [ADataCon dc | tc <- all_new_tcs
                                        , dc <- tyConDataCons tc])

      return (new_env, map mkLocalFamInst (repr_tcs ++ parr_tcs), concat binds)
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

      liftDs $ buildAlgTyCon name'
                             tyvars
                             []           -- no stupid theta
                             rhs'
                             rec_flag     -- FIXME: is this ok?
                             False        -- FIXME: no generics
                             False        -- not GADT syntax
                             Nothing      -- not a family instance
  where
    name   = tyConName tc
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

      liftDs $ buildDataCon name'
                            False           -- not infix
                            (map (const NotMarkedStrict) arg_tys)
                            []              -- no labelled fields
                            univ_tvs
                            []              -- no existential tvs for now
                            []              -- no eq spec for now
                            []              -- no context
                            arg_tys
                            tycon'
  where
    name        = dataConName dc
    univ_tvs    = dataConUnivTyVars dc
    rep_arg_tys = dataConRepArgTys dc
    tycon       = dataConTyCon dc

mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])

buildPReprTyCon :: TyCon -> TyCon -> VM TyCon
buildPReprTyCon orig_tc vect_tc
  = do
      name     <- cloneName mkPReprTyConOcc (tyConName orig_tc)
      rhs_ty   <- buildPReprType vect_tc
      prepr_tc <- builtin preprTyCon
      liftDs $ buildSynTyCon name
                             tyvars
                             (SynonymTyCon rhs_ty)
                             (Just $ mk_fam_inst prepr_tc vect_tc)
  where
    tyvars = tyConTyVars vect_tc

data TyConRepr = ProdRepr {
                   repr_prod_arg_tys      :: [Type]
                 , repr_prod_tycon        :: TyCon
                 , repr_prod_data_con     :: DataCon
                 , repr_prod_arr_tycon    :: TyCon
                 , repr_prod_arr_data_con :: DataCon
                 , repr_type              :: Type
                 }
               | SumRepr {
                   repr_tys            :: [[Type]]
                 , repr_prod_tycons    :: [Maybe TyCon]
                 , repr_prod_data_cons :: [Maybe DataCon]
                 , repr_prod_tys       :: [Type]
                 , repr_sum_tycon      :: TyCon
                 , repr_type           :: Type
                 }

arrShapeTys :: TyConRepr -> VM [Type]
arrShapeTys (ProdRepr {}) = return [intPrimTy]
arrShapeTys (SumRepr  {})
  = do
      uarr <- builtin uarrTyCon
      return [intPrimTy, mkTyConApp uarr [intTy]]

arrReprTys :: TyConRepr -> VM [Type]
arrReprTys (ProdRepr { repr_prod_arg_tys = tys })
  = mapM mkPArrayType tys
arrReprTys (SumRepr { repr_tys = tys })
  = concat `liftM` mapM (mapM mkPArrayType) (map mk_prod tys)
  where
    mk_prod []  = [unitTy]
    mk_prod tys = tys
      

mkTyConRepr :: TyCon -> VM TyConRepr
mkTyConRepr vect_tc
  | is_product
  = let
      [prod_arg_tys] = repr_tys
      arity          = length prod_arg_tys
    in
    do
      prod_tycon <- builtin (prodTyCon arity)
      let [prod_data_con] = tyConDataCons prod_tycon

      (arr_tycon, _) <- parrayReprTyCon
                      . mkTyConApp prod_tycon
                      $ replicate arity unitTy

      let [arr_data_con] = tyConDataCons arr_tycon

      return $ ProdRepr {
                 repr_prod_arg_tys      = prod_arg_tys
               , repr_prod_tycon        = prod_tycon
               , repr_prod_data_con     = prod_data_con
               , repr_prod_arr_tycon    = arr_tycon
               , repr_prod_arr_data_con = arr_data_con
               , repr_type              = mkTyConApp prod_tycon prod_arg_tys
               }

  | otherwise
  = do
      uarr <- builtin uarrTyCon
      prod_tycons  <- mapM (mk_tycon prodTyCon) repr_tys
      let prod_tys = zipWith mk_tc_app_maybe prod_tycons repr_tys
      sum_tycon    <- builtin (sumTyCon $ length repr_tys)
      arr_repr_tys <- mapM (mapM mkPArrayType . arr_repr_elem_tys) repr_tys

      return $ SumRepr {
                 repr_tys            = repr_tys
               , repr_prod_tycons    = prod_tycons
               , repr_prod_data_cons = map (fmap mk_single_datacon) prod_tycons
               , repr_prod_tys       = prod_tys
               , repr_sum_tycon      = sum_tycon
               , repr_type           = mkTyConApp sum_tycon prod_tys
               }
  where
    tyvars    = tyConTyVars vect_tc
    data_cons = tyConDataCons vect_tc
    repr_tys  = map dataConRepArgTys data_cons

    is_product | [_] <- data_cons = True
               | otherwise        = False

    mk_shape uarr = intPrimTy : mk_sel uarr

    mk_sel uarr | is_product = []
                | otherwise  = [uarr_int, uarr_int]
      where
        uarr_int = mkTyConApp uarr [intTy]

    mk_tycon get_tc tys
      | n > 1     = builtin (Just . get_tc n)
      | otherwise = return Nothing
      where n = length tys

    mk_single_datacon tc | [dc] <- tyConDataCons tc = dc

    mk_tc_app_maybe Nothing   []   = unitTy
    mk_tc_app_maybe Nothing   [ty] = ty
    mk_tc_app_maybe (Just tc) tys  = mkTyConApp tc tys

    arr_repr_elem_tys []  = [unitTy]
    arr_repr_elem_tys tys = tys

buildPReprType :: TyCon -> VM Type
buildPReprType = liftM repr_type . mkTyConRepr

buildToPRepr :: TyConRepr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildToPRepr (ProdRepr {
                repr_prod_arg_tys  = prod_arg_tys
              , repr_prod_data_con = prod_data_con
              , repr_type          = repr_type
              })
             vect_tc prepr_tc _
  = do
      arg  <- newLocalVar FSLIT("x") arg_ty
      vars <- mapM (newLocalVar FSLIT("x")) prod_arg_tys

      return . Lam arg
             . wrapFamInstBody prepr_tc var_tys
             $ Case (Var arg) (mkWildId arg_ty) repr_type
               [(DataAlt data_con, vars,
                 mkConApp prod_data_con (map Type prod_arg_tys ++ map Var vars))]
  where
    var_tys    = mkTyVarTys $ tyConTyVars vect_tc
    arg_ty     = mkTyConApp vect_tc var_tys
    [data_con] = tyConDataCons vect_tc

buildToPRepr (SumRepr {
                repr_tys            = repr_tys
              , repr_prod_data_cons = prod_data_cons
              , repr_prod_tys       = prod_tys
              , repr_sum_tycon      = sum_tycon
              , repr_type           = repr_type
              })
              vect_tc prepr_tc _
  = do
      arg  <- newLocalVar FSLIT("x") arg_ty
      vars <- mapM (mapM (newLocalVar FSLIT("x"))) repr_tys

      return . Lam arg
             . wrapFamInstBody prepr_tc var_tys
             . Case (Var arg) (mkWildId arg_ty) repr_type
             . zipWith4 mk_alt data_cons vars sum_data_cons
             . zipWith3 mk_prod prod_data_cons repr_tys $ map (map Var) vars
  where
    var_tys   = mkTyVarTys $ tyConTyVars vect_tc
    arg_ty    = mkTyConApp vect_tc var_tys
    data_cons = tyConDataCons vect_tc

    sum_data_cons = tyConDataCons sum_tycon

    mk_alt dc vars sum_dc expr = (DataAlt dc, vars,
                                  mkConApp sum_dc (map Type prod_tys ++ [expr]))

    mk_prod _         _   []     = Var unitDataConId
    mk_prod _         _   [expr] = expr
    mk_prod (Just dc) tys exprs  = mkConApp dc (map Type tys ++ exprs)

buildFromPRepr :: TyConRepr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildFromPRepr (ProdRepr {
                  repr_prod_arg_tys  = prod_arg_tys
                , repr_prod_data_con = prod_data_con
                , repr_type          = repr_type
                })
             vect_tc prepr_tc _
  = do
      arg_ty <- mkPReprType res_ty
      arg    <- newLocalVar FSLIT("x") arg_ty
      vars   <- mapM (newLocalVar FSLIT("x")) prod_arg_tys

      return . Lam arg
             $ Case (unwrapFamInstScrut prepr_tc var_tys (Var arg))
                    (mkWildId repr_type)
                    res_ty
               [(DataAlt prod_data_con, vars,
                 mkConApp data_con (map Type var_tys ++ map Var vars))]
  where
    var_tys    = mkTyVarTys $ tyConTyVars vect_tc
    ty_args    = map Type var_tys
    res_ty     = mkTyConApp vect_tc var_tys
    [data_con] = tyConDataCons vect_tc

buildFromPRepr (SumRepr {
                repr_tys            = repr_tys
              , repr_prod_data_cons = prod_data_cons
              , repr_prod_tys       = prod_tys
              , repr_sum_tycon      = sum_tycon
              , repr_type           = repr_type
              })
              vect_tc prepr_tc _
  = do
      arg_ty <- mkPReprType res_ty
      arg    <- newLocalVar FSLIT("x") arg_ty

      liftM (Lam arg
             . Case (unwrapFamInstScrut prepr_tc var_tys (Var arg))
                    (mkWildId repr_type)
                    res_ty
             . zipWith mk_alt sum_data_cons)
            (sequence
             $ zipWith4 un_prod data_cons prod_data_cons prod_tys repr_tys)
  where
    var_tys   = mkTyVarTys $ tyConTyVars vect_tc
    ty_args   = map Type var_tys
    res_ty    = mkTyConApp vect_tc var_tys
    data_cons = tyConDataCons vect_tc

    sum_data_cons = tyConDataCons sum_tycon

    un_prod dc _ _ []
      = do
          var <- newLocalVar FSLIT("u") unitTy
          return (var, mkConApp dc ty_args)
    un_prod dc _ _ [ty]
      = do
          var <- newLocalVar FSLIT("x") ty
          return (var, mkConApp dc (ty_args ++ [Var var]))

    un_prod dc (Just prod_dc) prod_ty tys
      = do
          vars  <- mapM (newLocalVar FSLIT("x")) tys
          pv    <- newLocalVar FSLIT("p") prod_ty

          let res  = mkConApp dc (ty_args ++ map Var vars)
              expr = Case (Var pv) (mkWildId prod_ty) res_ty
                        [(DataAlt prod_dc, vars, res)]

          return (pv, expr)

    mk_alt sum_dc (var, expr) = (DataAlt sum_dc, [var], expr)


buildToArrPRepr :: TyConRepr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildToArrPRepr repr@(ProdRepr {
                   repr_prod_arg_tys      = prod_arg_tys
                 , repr_prod_arr_tycon    = prod_arr_tycon
                 , repr_prod_arr_data_con = prod_arr_data_con
                 , repr_type              = repr_type
                 })
                vect_tc prepr_tc arr_tc
  = do
      arg_ty     <- mkPArrayType el_ty
      shape_tys  <- arrShapeTys repr
      arr_tys    <- arrReprTys repr
      res_ty     <- mkPArrayType repr_type
      rep_el_ty  <- mkPReprType el_ty

      arg        <- newLocalVar FSLIT("xs") arg_ty
      shape_vars <- mapM (newLocalVar FSLIT("sh")) shape_tys
      rep_vars   <- mapM (newLocalVar FSLIT("ys")) arr_tys
      
      let vars = shape_vars ++ rep_vars

      parray_co  <- mkBuiltinCo parrayTyCon

      let res = wrapFamInstBody prod_arr_tycon prod_arg_tys
              . mkConApp prod_arr_data_con
              $ map Type prod_arg_tys ++ map Var vars

          Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co  = mkAppCoercion parray_co
              . mkSymCoercion
              $ mkTyConApp repr_co var_tys

      return . Lam arg
             . mkCoerce co
             $ Case (unwrapFamInstScrut arr_tc var_tys (Var arg))
                    (mkWildId (mkTyConApp arr_tc var_tys))
                    res_ty
               [(DataAlt arr_dc, vars, res)]
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc var_tys

    [arr_dc] = tyConDataCons arr_tc


buildToArrPRepr _ _ _ _ = return (Var unitDataConId)
{-
buildToArrPRepr _ vect_tc prepr_tc arr_tc
  = do
      arg_ty  <- mkPArrayType el_ty
      rep_tys <- mapM (mapM mkPArrayType) rep_el_tys

      arg     <- newLocalVar FSLIT("xs") arg_ty
      bndrss  <- mapM (mapM (newLocalVar FSLIT("ys"))) rep_tys
      len     <- newLocalVar FSLIT("len") intPrimTy
      sel     <- newLocalVar FSLIT("sel") =<< mkPArrayType intTy

      let add_sel xs | has_selector = sel : xs
                     | otherwise    = xs

          all_bndrs = len : add_sel (concat bndrss)

      res      <- parrayCoerce prepr_tc var_tys
                =<< mkToArrPRepr (Var len) (Var sel) (map (map Var) bndrss)
      res_ty   <- mkPArrayType =<< mkPReprType el_ty

      return . Lam arg
             $ Case (unwrapFamInstScrut arr_tc var_tys (Var arg))
                    (mkWildId (mkTyConApp arr_tc var_tys))
                    res_ty
                    [(DataAlt arr_dc, all_bndrs, res)]
  where
    var_tys    = mkTyVarTys $ tyConTyVars vect_tc
    el_ty      = mkTyConApp vect_tc var_tys
    data_cons  = tyConDataCons vect_tc
    rep_el_tys = map dataConRepArgTys data_cons

    [arr_dc]   = tyConDataCons arr_tc

    has_selector | [_] <- data_cons = False
                 | otherwise        = True
-}

buildFromArrPRepr :: TyConRepr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildFromArrPRepr repr@(ProdRepr {
                          repr_prod_arg_tys      = prod_arg_tys
                        , repr_prod_arr_tycon    = prod_arr_tycon
                        , repr_prod_arr_data_con = prod_arr_data_con
                        , repr_type              = repr_type
                        })
                       vect_tc prepr_tc arr_tc
  = do
      rep_el_ty  <- mkPReprType el_ty
      arg_ty     <- mkPArrayType rep_el_ty
      shape_tys  <- arrShapeTys repr
      arr_tys    <- arrReprTys repr
      res_ty     <- mkPArrayType el_ty

      arg        <- newLocalVar FSLIT("xs") arg_ty
      shape_vars <- mapM (newLocalVar FSLIT("sh")) shape_tys
      rep_vars   <- mapM (newLocalVar FSLIT("ys")) arr_tys

      let vars = shape_vars ++ rep_vars

      parray_co  <- mkBuiltinCo parrayTyCon

      let res = wrapFamInstBody arr_tc var_tys
              . mkConApp arr_dc
              $ map Type var_tys ++ map Var vars

          Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co  = mkAppCoercion parray_co
              $ mkTyConApp repr_co var_tys

          scrut = unwrapFamInstScrut prod_arr_tycon prod_arg_tys
                $ mkCoerce co (Var arg)

      return . Lam arg
             $ Case (scrut)
                    (mkWildId (mkTyConApp prod_arr_tycon prod_arg_tys))
                    res_ty
               [(DataAlt prod_arr_data_con, vars, res)]
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc var_tys

    [arr_dc] = tyConDataCons arr_tc
buildFromArrPRepr _ _ _ _ = return (Var unitDataConId)

buildPRDictRepr :: TyConRepr -> VM CoreExpr
buildPRDictRepr (ProdRepr {
                   repr_prod_arg_tys = prod_arg_tys
                 , repr_prod_tycon   = prod_tycon
                 })
  = do
      prs  <- mapM mkPR prod_arg_tys
      dfun <- prDFunOfTyCon prod_tycon
      return $ dfun `mkTyApps` prod_arg_tys `mkApps` prs

buildPRDictRepr (SumRepr {
                   repr_tys         = repr_tys
                 , repr_prod_tycons = prod_tycons
                 , repr_prod_tys    = prod_tys
                 , repr_sum_tycon   = sum_tycon
                 })
  = do
      prs      <- mapM (mapM mkPR) repr_tys
      prod_prs <- sequence $ zipWith3 mk_prod_pr prod_tycons repr_tys prs
      sum_dfun <- prDFunOfTyCon sum_tycon
      return $ sum_dfun `mkTyApps` prod_tys `mkApps` prod_prs
  where
    mk_prod_pr _         _   []   = prDFunOfTyCon unitTyCon
    mk_prod_pr _         _   [pr] = return pr
    mk_prod_pr (Just tc) tys prs
      = do
          dfun <- prDFunOfTyCon tc
          return $ dfun `mkTyApps` tys `mkApps` prs

buildPRDict :: TyConRepr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildPRDict repr vect_tc prepr_tc _
  = do
      dict  <- buildPRDictRepr repr

      pr_co <- mkBuiltinCo prTyCon
      let co = mkAppCoercion pr_co
             . mkSymCoercion
             $ mkTyConApp arg_co var_tys

      return $ mkCoerce co dict
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc

    Just arg_co = tyConFamilyCoercion_maybe prepr_tc

buildPArrayTyCon :: TyCon -> TyCon -> VM TyCon
buildPArrayTyCon orig_tc vect_tc = fixV $ \repr_tc ->
  do
    name'  <- cloneName mkPArrayTyConOcc orig_name
    rhs    <- buildPArrayTyConRhs orig_name vect_tc repr_tc
    parray <- builtin parrayTyCon

    liftDs $ buildAlgTyCon name'
                           tyvars
                           []          -- no stupid theta
                           rhs
                           rec_flag    -- FIXME: is this ok?
                           False       -- FIXME: no generics
                           False       -- not GADT syntax
                           (Just $ mk_fam_inst parray vect_tc)
  where
    orig_name = tyConName orig_tc
    tyvars = tyConTyVars vect_tc
    rec_flag = boolToRecFlag (isRecursiveTyCon vect_tc)
    

buildPArrayTyConRhs :: Name -> TyCon -> TyCon -> VM AlgTyConRhs
buildPArrayTyConRhs orig_name vect_tc repr_tc
  = do
      data_con <- buildPArrayDataCon orig_name vect_tc repr_tc
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPArrayDataCon :: Name -> TyCon -> TyCon -> VM DataCon
buildPArrayDataCon orig_name vect_tc repr_tc
  = do
      dc_name  <- cloneName mkPArrayDataConOcc orig_name
      repr     <- mkTyConRepr vect_tc

      shape_tys <- arrShapeTys repr
      repr_tys  <- arrReprTys  repr

      let tys = shape_tys ++ repr_tys

      liftDs $ buildDataCon dc_name
                            False                  -- not infix
                            (map (const NotMarkedStrict) tys)
                            []                     -- no field labels
                            (tyConTyVars vect_tc)
                            []                     -- no existentials
                            []                     -- no eq spec
                            []                     -- no context
                            tys
                            repr_tc

mkPADFun :: TyCon -> VM Var
mkPADFun vect_tc
  = newExportedVar (mkPADFunOcc $ getOccName vect_tc) =<< paDFunType vect_tc

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

buildTyConBindings :: TyCon -> TyCon -> TyCon -> TyCon -> Var
                   -> VM [(Var, CoreExpr)]
buildTyConBindings orig_tc vect_tc prepr_tc arr_tc dfun
  = do
      shape <- tyConShape vect_tc
      repr  <- mkTyConRepr vect_tc
      sequence_ (zipWith4 (vectDataConWorker shape vect_tc arr_tc arr_dc)
                          orig_dcs
                          vect_dcs
                          (inits repr_tys)
                          (tails repr_tys))
      dict <- buildPADict repr vect_tc prepr_tc arr_tc dfun
      binds <- takeHoisted
      return $ (dfun, dict) : binds
  where
    orig_dcs = tyConDataCons orig_tc
    vect_dcs = tyConDataCons vect_tc
    [arr_dc] = tyConDataCons arr_tc

    repr_tys = map dataConRepArgTys vect_dcs

vectDataConWorker :: Shape -> TyCon -> TyCon -> DataCon
                  -> DataCon -> DataCon -> [[Type]] -> [[Type]]
                  -> VM ()
vectDataConWorker shape vect_tc arr_tc arr_dc orig_dc vect_dc pre (dc_tys : post)
  = do
      clo <- closedV
           . inBind orig_worker
           . polyAbstract tvs $ \abstract ->
             liftM (abstract . vectorised)
           $ buildClosures tvs [] dc_tys res_ty (liftM2 (,) mk_vect mk_lift)

      worker <- cloneId mkVectOcc orig_worker (exprType clo)
      hoistBinding worker clo
      defGlobalVar orig_worker worker
      return ()
  where
    tvs     = tyConTyVars vect_tc
    arg_tys = mkTyVarTys tvs
    res_ty  = mkTyConApp vect_tc arg_tys

    orig_worker = dataConWorkId orig_dc

    mk_vect = return . mkConApp vect_dc $ map Type arg_tys
    mk_lift = do
                len     <- newLocalVar FSLIT("n") intPrimTy
                arr_tys <- mapM mkPArrayType dc_tys
                args    <- mapM (newLocalVar FSLIT("xs")) arr_tys
                shapes  <- shapeReplicate shape
                                          (Var len)
                                          (mkDataConTag vect_dc)
                
                empty_pre  <- mapM emptyPA (concat pre)
                empty_post <- mapM emptyPA (concat post)

                return . mkLams (len : args)
                       . wrapFamInstBody arr_tc arg_tys
                       . mkConApp arr_dc
                       $ map Type arg_tys ++ shapes
                                          ++ empty_pre
                                          ++ map Var args
                                          ++ empty_post

buildPADict :: TyConRepr -> TyCon -> TyCon -> TyCon -> Var -> VM CoreExpr
buildPADict repr vect_tc prepr_tc arr_tc dfun
  = polyAbstract tvs $ \abstract ->
    do
      meth_binds <- mapM (mk_method repr) paMethods
      let meth_exprs = map (Var . fst) meth_binds

      pa_dc <- builtin paDataCon
      let dict = mkConApp pa_dc (Type (mkTyConApp vect_tc arg_tys) : meth_exprs)
          body = Let (Rec meth_binds) dict
      return . mkInlineMe $ abstract body
  where
    tvs = tyConTyVars arr_tc
    arg_tys = mkTyVarTys tvs

    mk_method repr (name, build)
      = localV
      $ do
          body <- build repr vect_tc prepr_tc arr_tc
          var  <- newLocalVar name (exprType body)
          return (var, mkInlineMe body)
          
paMethods = [(FSLIT("toPRepr"),      buildToPRepr),
             (FSLIT("fromPRepr"),    buildFromPRepr),
             (FSLIT("toArrPRepr"),   buildToArrPRepr),
             (FSLIT("fromArrPRepr"), buildFromArrPRepr),
             (FSLIT("dictPRepr"),    buildPRDict)]

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


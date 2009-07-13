module VectType ( vectTyCon, vectAndLiftType, vectType, vectTypeEnv,
                  -- arrSumArity, pdataCompTys, pdataCompVars,
                  buildPADict,
                  fromVect )
where

import VectMonad
import VectUtils
import VectCore

import HscTypes          ( TypeEnv, extendTypeEnvList, typeEnvTyCons )
import CoreSyn
import CoreUtils
import MkCore		 ( mkWildCase )
import BuildTyCl
import DataCon
import TyCon
import Type
import TypeRep
import Coercion
import FamInstEnv        ( FamInst, mkLocalFamInst )
import OccName
import MkId
import BasicTypes        ( StrictnessMark(..), boolToRecFlag )
import Var               ( Var, TyVar )
import Name              ( Name, getOccName )
import NameEnv

import Unique
import UniqFM
import UniqSet
import Util
import Digraph           ( SCC(..), stronglyConnCompFromEdgedVertices )

import Outputable
import FastString

import Control.Monad  ( liftM, liftM2, zipWithM, zipWithM_, mapAndUnzipM )
import Data.List      ( inits, tails, zipWith4, zipWith5 )

-- ----------------------------------------------------------------------------
-- Types

vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise            = maybeCantVectoriseM "Tycon not vectorised:" (ppr tc)
                         $ lookupTyCon tc

vectAndLiftType :: Type -> VM (Type, Type)
vectAndLiftType ty | Just ty' <- coreView ty = vectAndLiftType ty'
vectAndLiftType ty
  = do
      mdicts   <- mapM paDictArgType tyvars
      let dicts = [dict | Just dict <- mdicts]
      vmono_ty <- vectType mono_ty
      lmono_ty <- mkPDataType vmono_ty
      return (abstractType tyvars dicts vmono_ty,
              abstractType tyvars dicts lmono_ty)
  where
    (tyvars, mono_ty) = splitForAllTys ty


vectType :: Type -> VM Type
vectType ty | Just ty' <- coreView ty = vectType ty'
vectType (TyVarTy tv) = return $ TyVarTy tv
vectType (AppTy ty1 ty2) = liftM2 AppTy (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) = liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   = liftM2 TyConApp (builtin closureTyCon)
                                             (mapM vectAndBoxType [ty1,ty2])
vectType ty@(ForAllTy _ _)
  = do
      mdicts   <- mapM paDictArgType tyvars
      mono_ty' <- vectType mono_ty
      return $ abstractType tyvars [dict | Just dict <- mdicts] mono_ty'
  where
    (tyvars, mono_ty) = splitForAllTys ty

vectType ty = cantVectorise "Can't vectorise type" (ppr ty)

vectAndBoxType :: Type -> VM Type
vectAndBoxType ty = vectType ty >>= boxType

abstractType :: [TyVar] -> [Type] -> Type -> Type
abstractType tyvars dicts = mkForAllTys tyvars . mkFunTys dicts

-- ----------------------------------------------------------------------------
-- Boxing

boxType :: Type -> VM Type
boxType ty
  | Just (tycon, []) <- splitTyConApp_maybe ty
  , isUnLiftedTyCon tycon
  = do
      r <- lookupBoxedTyCon tycon
      case r of
        Just tycon' -> return $ mkTyConApp tycon' []
        Nothing     -> return ty
boxType ty = return ty

-- ----------------------------------------------------------------------------
-- Type definitions

type TyConGroup = ([TyCon], UniqSet TyCon)

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
          vect_tcs = keep_tcs ++ new_tcs

      repr_tcs  <- zipWithM buildPReprTyCon orig_tcs vect_tcs
      pdata_tcs <- zipWithM buildPDataTyCon orig_tcs vect_tcs
      dfuns     <- mapM mkPADFun vect_tcs
      defTyConPAs (zip vect_tcs dfuns)
      binds    <- sequence (zipWith5 buildTyConBindings orig_tcs
                                                        vect_tcs
                                                        repr_tcs
                                                        pdata_tcs
                                                        dfuns)

      let all_new_tcs = new_tcs ++ repr_tcs ++ pdata_tcs

      let new_env = extendTypeEnvList env
                       (map ATyCon all_new_tcs
                        ++ [ADataCon dc | tc <- all_new_tcs
                                        , dc <- tyConDataCons tc])

      return (new_env, map mkLocalFamInst (repr_tcs ++ pdata_tcs), concat binds)
  where
    tycons = typeEnvTyCons env
    groups = tyConGroups tycons

    mk_map env = listToUFM_Directly [(u, getUnique n /= u) | (u,n) <- nameEnvUniqueElts env]


vectTyConDecls :: [TyCon] -> VM [TyCon]
vectTyConDecls tcs = fixV $ \tcs' ->
  do
    mapM_ (uncurry defTyCon) (zipLazy tcs tcs')
    mapM vectTyConDecl tcs

vectTyConDecl :: TyCon -> VM TyCon
vectTyConDecl tc
  = do
      name' <- cloneName mkVectTyConOcc name
      rhs'  <- vectAlgTyConRhs tc (algTyConRhs tc)

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

vectAlgTyConRhs :: TyCon -> AlgTyConRhs -> VM AlgTyConRhs
vectAlgTyConRhs _ (DataTyCon { data_cons = data_cons
                             , is_enum   = is_enum
                             })
  = do
      data_cons' <- mapM vectDataCon data_cons
      zipWithM_ defDataCon data_cons data_cons'
      return $ DataTyCon { data_cons = data_cons'
                         , is_enum   = is_enum
                         }
vectAlgTyConRhs tc _ = cantVectorise "Can't vectorise type definition:" (ppr tc)

vectDataCon :: DataCon -> VM DataCon
vectDataCon dc
  | not . null $ dataConExTyVars dc
        = cantVectorise "Can't vectorise constructor (existentials):" (ppr dc)
  | not . null $ dataConEqSpec   dc
        = cantVectorise "Can't vectorise constructor (eq spec):" (ppr dc)
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
			    (mkFamilyTyConApp tycon' (mkTyVarTys univ_tvs))
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
			     (typeKind rhs_ty)
                             (Just $ mk_fam_inst prepr_tc vect_tc)
  where
    tyvars = tyConTyVars vect_tc

buildPReprType :: TyCon -> VM Type
buildPReprType vect_tc = sum_type . map dataConRepArgTys $ tyConDataCons vect_tc
  where
    sum_type []    = voidType
    sum_type [tys] = prod_type tys
    sum_type _     = do
                       (sum_tc, _, _, args) <- reprSumTyCons vect_tc
                       return $ mkTyConApp sum_tc args

    prod_type []   = voidType
    prod_type [ty] = return ty
    prod_type tys  = do
                       prod_tc <- builtin (prodTyCon (length tys))
                       return $ mkTyConApp prod_tc tys

reprSumTyCons :: TyCon -> VM (TyCon, TyCon, Type, [Type])
reprSumTyCons vect_tc
  = do
      tc   <- builtin (sumTyCon arity)
      args <- mapM (prod . dataConRepArgTys) cons
      (pdata_tc, _) <- pdataReprTyCon (mkTyConApp tc args)
      sel_ty <- builtin (selTy arity)
      return (tc, pdata_tc, sel_ty, args)
  where
    cons = tyConDataCons vect_tc
    arity = length cons

    prod []   = voidType
    prod [ty] = return ty
    prod tys  = do
                  prod_tc <- builtin (prodTyCon (length tys))
                  return $ mkTyConApp prod_tc tys

buildToPRepr :: TyCon -> TyCon -> TyCon -> VM CoreExpr
buildToPRepr vect_tc repr_tc _
  = do
      let arg_ty = mkTyConApp vect_tc ty_args
      res_ty <- mkPReprType arg_ty
      arg    <- newLocalVar (fsLit "x") arg_ty
      result <- to_sum (Var arg) arg_ty res_ty (tyConDataCons vect_tc)
      return $ Lam arg result
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)

    wrap = wrapFamInstBody repr_tc ty_args

    to_sum _ _ _ []
      = do
          void <- builtin voidVar
          return $ wrap (Var void)

    to_sum arg arg_ty res_ty [con]
      = do
          (prod, vars) <- to_prod (dataConRepArgTys con)
          return $ mkWildCase arg arg_ty res_ty
                   [(DataAlt con, vars, wrap prod)]

    to_sum arg arg_ty res_ty cons
      = do
          (prods, vars) <- mapAndUnzipM (to_prod . dataConRepArgTys) cons
          (sum_tc, _, _, sum_ty_args) <- reprSumTyCons vect_tc
          let sum_cons = [mkConApp con (map Type sum_ty_args)
                            | con <- tyConDataCons sum_tc]
          return . mkWildCase arg arg_ty res_ty
                 $ zipWith4 mk_alt cons vars sum_cons prods
      where
        mk_alt con vars sum_con expr
          = (DataAlt con, vars, wrap $ sum_con `App` expr)

    to_prod []
      = do
          void <- builtin voidVar
          return (Var void, [])
    to_prod [ty]
      = do
          var <- newLocalVar (fsLit "x") ty
          return (Var var, [var])
    to_prod tys
      = do
          prod_con <- builtin (prodDataCon (length tys))
          vars <- newLocalVars (fsLit "x") tys
          return (mkConApp prod_con (map Type tys ++ map Var vars), vars)

buildFromPRepr :: TyCon -> TyCon -> TyCon -> VM CoreExpr
buildFromPRepr vect_tc repr_tc _
  = do
      arg_ty <- mkPReprType res_ty
      arg <- newLocalVar (fsLit "x") arg_ty

      result <- from_sum (unwrapFamInstScrut repr_tc ty_args (Var arg))
                         (tyConDataCons vect_tc)
      return $ Lam arg result
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)
    res_ty  = mkTyConApp vect_tc ty_args

    from_sum _    []    = pprPanic "buildFromPRepr" (ppr vect_tc)
    from_sum expr [con] = from_prod expr con
    from_sum expr cons
      = do
          (sum_tc, _, _, sum_ty_args) <- reprSumTyCons vect_tc
          let sum_cons = tyConDataCons sum_tc
          vars <- newLocalVars (fsLit "x") sum_ty_args
          prods <- zipWithM from_prod (map Var vars) cons
          return . mkWildCase expr (exprType expr) res_ty
                 $ zipWith3 mk_alt sum_cons vars prods
      where
        mk_alt con var expr = (DataAlt con, [var], expr)

    from_prod expr con
      = case dataConRepArgTys con of
          []   -> return $ apply_con []
          [_]  -> return $ apply_con [expr]
          tys  -> do
                    prod_con <- builtin (prodDataCon (length tys))
                    vars <- newLocalVars (fsLit "y") tys
                    return $ mkWildCase expr (exprType expr) res_ty
                             [(DataAlt prod_con, vars, apply_con (map Var vars))]
      where
        apply_con exprs = mkConApp con (map Type ty_args) `mkApps` exprs

buildToArrPRepr :: TyCon -> TyCon -> TyCon -> VM CoreExpr
buildToArrPRepr vect_tc prepr_tc pdata_tc
  = do
      arg_ty <- mkPDataType el_ty
      res_ty <- mkPDataType =<< mkPReprType el_ty
      arg    <- newLocalVar (fsLit "xs") arg_ty

      pdata_co <- mkBuiltinCo pdataTyCon
      let Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co           = mkAppCoercion pdata_co
                       . mkSymCoercion
                       $ mkTyConApp repr_co ty_args

          scrut   = unwrapFamInstScrut pdata_tc ty_args (Var arg)

      (vars, result) <- to_sum (tyConDataCons vect_tc)

      return . Lam arg
             $ mkWildCase scrut (mkTyConApp pdata_tc ty_args) res_ty
               [(DataAlt pdata_dc, vars, mkCoerce co result)]
  where
    ty_args = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc ty_args

    [pdata_dc] = tyConDataCons pdata_tc

    to_sum []    = do
                     pvoid <- builtin pvoidVar
                     return ([], Var pvoid)
    to_sum [con] = to_prod con
    to_sum cons  = do
                     (vars, exprs) <- mapAndUnzipM to_prod cons
                     (_, pdata_tc, sel_ty, arg_tys) <- reprSumTyCons vect_tc
                     sel <- newLocalVar (fsLit "sel") sel_ty
                     let [pdata_con] = tyConDataCons pdata_tc
                         result = wrapFamInstBody pdata_tc arg_tys
                                . mkConApp pdata_con
                                $ map Type arg_tys ++ (Var sel : exprs)
                     return (sel : concat vars, result)

    to_prod con
      | [] <- tys = do
                      pvoid <- builtin pvoidVar
                      return ([], Var pvoid)
      | [ty] <- tys = do
                        var <- newLocalVar (fsLit "x") ty
                        return ([var], Var var)
      | otherwise
        = do
            vars <- newLocalVars (fsLit "x") tys
            prod_tc <- builtin (prodTyCon (length tys))
            (pdata_prod_tc, _) <- pdataReprTyCon (mkTyConApp prod_tc tys)
            let [pdata_prod_con] = tyConDataCons pdata_prod_tc
                result = wrapFamInstBody pdata_prod_tc tys
                       . mkConApp pdata_prod_con
                       $ map Type tys ++ map Var vars
            return (vars, result)
      where
        tys = dataConRepArgTys con

buildFromArrPRepr :: TyCon -> TyCon -> TyCon -> VM CoreExpr
buildFromArrPRepr vect_tc prepr_tc pdata_tc
  = do
      arg_ty <- mkPDataType =<< mkPReprType el_ty
      res_ty <- mkPDataType el_ty
      arg    <- newLocalVar (fsLit "xs") arg_ty

      pdata_co <- mkBuiltinCo pdataTyCon
      let Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co           = mkAppCoercion pdata_co
                       $ mkTyConApp repr_co var_tys

          scrut  = mkCoerce co (Var arg)

      (args, mk) <- from_sum res_ty scrut (tyConDataCons vect_tc)
      
      let result = wrapFamInstBody pdata_tc var_tys
                 . mkConApp pdata_dc
                 $ map Type var_tys ++ args

      return $ Lam arg (mk result)
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc var_tys

    [pdata_dc] = tyConDataCons pdata_tc

    from_sum res_ty expr [] = return ([], mk)
      where
        mk body = mkWildCase expr (exprType expr) res_ty [(DEFAULT, [], body)]
    from_sum res_ty expr [con] = from_prod res_ty expr con
    from_sum res_ty expr cons
      = do
          (_, pdata_tc, sel_ty, arg_tys) <- reprSumTyCons vect_tc
          sel  <- newLocalVar (fsLit "sel") sel_ty
          vars <- newLocalVars (fsLit "xs") arg_tys
          rs   <- zipWithM (from_prod res_ty) (map Var vars) cons
          let (prods, mks) = unzip rs
              [pdata_con]  = tyConDataCons pdata_tc
              scrut        = unwrapFamInstScrut pdata_tc arg_tys expr

              mk body = mkWildCase scrut (exprType scrut) res_ty
                        [(DataAlt pdata_con, sel : vars, foldr ($) body mks)]
          return (Var sel : concat prods, mk)


    from_prod res_ty expr con
      | []  <- tys = return ([], id)
      | [_] <- tys = return ([expr], id)
      | otherwise
        = do
            prod_tc <- builtin (prodTyCon (length tys))
            (pdata_tc, _) <- pdataReprTyCon (mkTyConApp prod_tc tys)
            pdata_tys <- mapM mkPDataType tys
            vars <- newLocalVars (fsLit "ys") pdata_tys
            let [pdata_con] = tyConDataCons pdata_tc
                scrut       = unwrapFamInstScrut pdata_tc tys expr

                mk body = mkWildCase scrut (exprType scrut) res_ty
                          [(DataAlt pdata_con, vars, body)]

            return (map Var vars, mk)
      where
        tys = dataConRepArgTys con

buildPRDict :: TyCon -> TyCon -> TyCon -> VM CoreExpr
buildPRDict vect_tc prepr_tc _
  = do
      dict <- sum_dict (tyConDataCons vect_tc)
      pr_co <- mkBuiltinCo prTyCon
      let co = mkAppCoercion pr_co
             . mkSymCoercion
             $ mkTyConApp arg_co ty_args
      return (mkCoerce co dict)
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)
    Just arg_co = tyConFamilyCoercion_maybe prepr_tc

    sum_dict []    = prDFunOfTyCon =<< builtin voidTyCon
    sum_dict [con] = prod_dict con
    sum_dict cons  = do
                       dicts <- mapM prod_dict cons
                       (sum_tc, _, _, sum_ty_args) <- reprSumTyCons vect_tc
                       dfun <- prDFunOfTyCon sum_tc
                       return $ dfun `mkTyApps` sum_ty_args `mkApps` dicts

    prod_dict con
      | []   <- tys = prDFunOfTyCon =<< builtin voidTyCon
      | [ty] <- tys = mkPR ty
      | otherwise   = do
                        dicts <- mapM mkPR tys
                        prod_tc <- builtin (prodTyCon (length tys))
                        dfun <- prDFunOfTyCon prod_tc
                        return $ dfun `mkTyApps` tys `mkApps` dicts
      where
        tys = dataConRepArgTys con

buildPDataTyCon :: TyCon -> TyCon -> VM TyCon
buildPDataTyCon orig_tc vect_tc = fixV $ \repr_tc ->
  do
    name' <- cloneName mkPDataTyConOcc orig_name
    rhs   <- buildPDataTyConRhs orig_name vect_tc repr_tc
    pdata <- builtin pdataTyCon

    liftDs $ buildAlgTyCon name'
                           tyvars
                           []          -- no stupid theta
                           rhs
                           rec_flag    -- FIXME: is this ok?
                           False       -- FIXME: no generics
                           False       -- not GADT syntax
                           (Just $ mk_fam_inst pdata vect_tc)
  where
    orig_name = tyConName orig_tc
    tyvars = tyConTyVars vect_tc
    rec_flag = boolToRecFlag (isRecursiveTyCon vect_tc)


buildPDataTyConRhs :: Name -> TyCon -> TyCon -> VM AlgTyConRhs
buildPDataTyConRhs orig_name vect_tc repr_tc
  = do
      data_con <- buildPDataDataCon orig_name vect_tc repr_tc
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPDataDataCon :: Name -> TyCon -> TyCon -> VM DataCon
buildPDataDataCon orig_name vect_tc repr_tc
  = do
      dc_name  <- cloneName mkPDataDataConOcc orig_name
      comp_tys <- components

      liftDs $ buildDataCon dc_name
                            False                  -- not infix
                            (map (const NotMarkedStrict) comp_tys)
                            []                     -- no field labels
                            tvs
                            []                     -- no existentials
                            []                     -- no eq spec
                            []                     -- no context
                            comp_tys
			    (mkFamilyTyConApp repr_tc (mkTyVarTys tvs))
                            repr_tc
  where
    tvs   = tyConTyVars vect_tc
    cons  = tyConDataCons vect_tc
    arity = length cons

    components
      | arity > 1 = liftM2 (:) (builtin (selTy arity)) data_components
      | otherwise = data_components

    data_components = mapM mkPDataType
                    . concat
                    $ map dataConRepArgTys cons

mkPADFun :: TyCon -> VM Var
mkPADFun vect_tc
  = newExportedVar (mkPADFunOcc $ getOccName vect_tc) =<< paDFunType vect_tc

buildTyConBindings :: TyCon -> TyCon -> TyCon -> TyCon -> Var
                   -> VM [(Var, CoreExpr)]
buildTyConBindings orig_tc vect_tc prepr_tc pdata_tc dfun
  = do
      vectDataConWorkers orig_tc vect_tc pdata_tc
      dict <- buildPADict vect_tc prepr_tc pdata_tc dfun
      binds <- takeHoisted
      return $ (dfun, dict) : binds

vectDataConWorkers :: TyCon -> TyCon -> TyCon -> VM ()
vectDataConWorkers orig_tc vect_tc arr_tc
  = do
      bs <- sequence
          . zipWith3 def_worker  (tyConDataCons orig_tc) rep_tys
          $ zipWith4 mk_data_con (tyConDataCons vect_tc)
                                 rep_tys
                                 (inits rep_tys)
                                 (tail $ tails rep_tys)
      mapM_ (uncurry hoistBinding) bs
  where
    tyvars   = tyConTyVars vect_tc
    var_tys  = mkTyVarTys tyvars
    ty_args  = map Type var_tys
    res_ty   = mkTyConApp vect_tc var_tys

    cons     = tyConDataCons vect_tc
    arity    = length cons
    [arr_dc] = tyConDataCons arr_tc

    rep_tys  = map dataConRepArgTys $ tyConDataCons vect_tc


    mk_data_con con tys pre post
      = liftM2 (,) (vect_data_con con)
                   (lift_data_con tys pre post (mkDataConTag con))

    sel_replicate len tag
      | arity > 1 = do
                      rep <- builtin (selReplicate arity)
                      return [rep `mkApps` [len, tag]]

      | otherwise = return []

    vect_data_con con = return $ mkConApp con ty_args
    lift_data_con tys pre_tys post_tys tag
      = do
          len  <- builtin liftingContext
          args <- mapM (newLocalVar (fsLit "xs"))
                  =<< mapM mkPDataType tys

          sel  <- sel_replicate (Var len) tag

          pre   <- mapM emptyPD (concat pre_tys)
          post  <- mapM emptyPD (concat post_tys)

          return . mkLams (len : args)
                 . wrapFamInstBody arr_tc var_tys
                 . mkConApp arr_dc
                 $ ty_args ++ sel ++ pre ++ map Var args ++ post

    def_worker data_con arg_tys mk_body
      = do
          body <- closedV
                . inBind orig_worker
                . polyAbstract tyvars $ \abstract ->
                  liftM (abstract . vectorised)
                $ buildClosures tyvars [] arg_tys res_ty mk_body

          vect_worker <- cloneId mkVectOcc orig_worker (exprType body)
          defGlobalVar orig_worker vect_worker
          return (vect_worker, body)
      where
        orig_worker = dataConWorkId data_con

buildPADict :: TyCon -> TyCon -> TyCon -> Var -> VM CoreExpr
buildPADict vect_tc prepr_tc arr_tc _
  = polyAbstract tvs $ \abstract ->
    do
      meth_binds <- mapM mk_method paMethods
      let meth_exprs = map (Var . fst) meth_binds

      pa_dc <- builtin paDataCon
      let dict = mkConApp pa_dc (Type (mkTyConApp vect_tc arg_tys) : meth_exprs)
          body = Let (Rec meth_binds) dict
      return . mkInlineMe $ abstract body
  where
    tvs = tyConTyVars arr_tc
    arg_tys = mkTyVarTys tvs

    mk_method (name, build)
      = localV
      $ do
          body <- build vect_tc prepr_tc arr_tc
          var  <- newLocalVar name (exprType body)
          return (var, mkInlineMe body)

paMethods :: [(FastString, TyCon -> TyCon -> TyCon -> VM CoreExpr)]
paMethods = [(fsLit "toPRepr",      buildToPRepr),
             (fsLit "fromPRepr",    buildFromPRepr),
             (fsLit "toArrPRepr",   buildToArrPRepr),
             (fsLit "fromArrPRepr", buildFromArrPRepr),
             (fsLit "dictPRepr",    buildPRDict)]

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
    classify conv keep _  [] = (conv, keep)
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
tyConGroups tcs = map mk_grp (stronglyConnCompFromEdgedVertices edges)
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
tyConsOfType (TyVarTy _)       = emptyUniqSet
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


-- ----------------------------------------------------------------------------
-- Conversions

fromVect :: Type -> CoreExpr -> VM CoreExpr
fromVect ty expr | Just ty' <- coreView ty = fromVect ty' expr
fromVect (FunTy arg_ty res_ty) expr
  = do
      arg     <- newLocalVar (fsLit "x") arg_ty
      varg    <- toVect arg_ty (Var arg)
      varg_ty <- vectType arg_ty
      vres_ty <- vectType res_ty
      apply   <- builtin applyVar
      body    <- fromVect res_ty
               $ Var apply `mkTyApps` [varg_ty, vres_ty] `mkApps` [expr, varg]
      return $ Lam arg body
fromVect ty expr
  = identityConv ty >> return expr

toVect :: Type -> CoreExpr -> VM CoreExpr
toVect ty expr = identityConv ty >> return expr

identityConv :: Type -> VM ()
identityConv ty | Just ty' <- coreView ty = identityConv ty'
identityConv (TyConApp tycon tys)
  = do
      mapM_ identityConv tys
      identityConvTyCon tycon
identityConv _ = noV

identityConvTyCon :: TyCon -> VM ()
identityConvTyCon tc
  | isBoxedTupleTyCon tc = return ()
  | isUnLiftedTyCon tc   = return ()
  | otherwise            = do
                             tc' <- maybeV (lookupTyCon tc)
                             if tc == tc' then return () else noV


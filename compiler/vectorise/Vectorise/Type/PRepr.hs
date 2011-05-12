
module Vectorise.Type.PRepr
	( buildPReprTyCon
	, buildToPRepr
	, buildFromPRepr
	, buildToArrPRepr
	, buildFromArrPRepr)
where
import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Repr
import CoreSyn
import CoreUtils
import MkCore		 ( mkWildCase )
import TyCon
import Type
import Kind
import BuildTyCl
import OccName
import Coercion
import MkId

import FastString
import MonadUtils
import Control.Monad


mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])


buildPReprTyCon :: TyCon -> TyCon -> SumRepr -> VM TyCon
buildPReprTyCon orig_tc vect_tc repr
  = do
      name     <- cloneName mkPReprTyConOcc (tyConName orig_tc)
      -- rhs_ty   <- buildPReprType vect_tc
      rhs_ty   <- sumReprType repr
      prepr_tc <- builtin preprTyCon
      liftDs $ buildSynTyCon name
                             tyvars
                             (SynonymTyCon rhs_ty)
			     (typeKind rhs_ty)
                             NoParentTyCon
                             (Just $ mk_fam_inst prepr_tc vect_tc)
  where
    tyvars = tyConTyVars vect_tc


buildToPRepr :: TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr
buildToPRepr vect_tc repr_tc _ repr
  = do
      let arg_ty = mkTyConApp vect_tc ty_args
      res_ty <- mkPReprType arg_ty
      arg    <- newLocalVar (fsLit "x") arg_ty
      result <- to_sum (Var arg) arg_ty res_ty repr
      return $ Lam arg result
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)

    wrap_repr_inst = wrapFamInstBody repr_tc ty_args

    to_sum _ _ _ EmptySum
      = do
          void <- builtin voidVar
          return $ wrap_repr_inst $ Var void

    to_sum arg arg_ty res_ty (UnarySum r)
      = do
          (pat, vars, body) <- con_alt r
          return $ mkWildCase arg arg_ty res_ty
                   [(pat, vars, wrap_repr_inst body)]

    to_sum arg arg_ty res_ty (Sum { repr_sum_tc  = sum_tc
                                  , repr_con_tys = tys
                                  , repr_cons    =  cons })
      = do
          alts <- mapM con_alt cons
          let alts' = [(pat, vars, wrap_repr_inst
                                   $ mkConApp sum_con (map Type tys ++ [body]))
                        | ((pat, vars, body), sum_con)
                            <- zip alts (tyConDataCons sum_tc)]
          return $ mkWildCase arg arg_ty res_ty alts'

    con_alt (ConRepr con r)
      = do
          (vars, body) <- to_prod r
          return (DataAlt con, vars, body)

    to_prod EmptyProd
      = do
          void <- builtin voidVar
          return ([], Var void)

    to_prod (UnaryProd comp)
      = do
          var  <- newLocalVar (fsLit "x") (compOrigType comp)
          body <- to_comp (Var var) comp
          return ([var], body)

    to_prod(Prod { repr_tup_tc   = tup_tc
                 , repr_comp_tys = tys
                 , repr_comps    = comps })
      = do
          vars  <- newLocalVars (fsLit "x") (map compOrigType comps)
          exprs <- zipWithM to_comp (map Var vars) comps
          return (vars, mkConApp tup_con (map Type tys ++ exprs))
      where
        [tup_con] = tyConDataCons tup_tc

    to_comp expr (Keep _ _) = return expr
    to_comp expr (Wrap ty)  = do
                                wrap_tc <- builtin wrapTyCon
                                return $ wrapNewTypeBody wrap_tc [ty] expr


buildFromPRepr :: TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr
buildFromPRepr vect_tc repr_tc _ repr
  = do
      arg_ty <- mkPReprType res_ty
      arg <- newLocalVar (fsLit "x") arg_ty

      result <- from_sum (unwrapFamInstScrut repr_tc ty_args (Var arg))
                         repr
      return $ Lam arg result
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)
    res_ty  = mkTyConApp vect_tc ty_args

    from_sum _ EmptySum
      = do
          dummy <- builtin fromVoidVar
          return $ Var dummy `App` Type res_ty

    from_sum expr (UnarySum r) = from_con expr r
    from_sum expr (Sum { repr_sum_tc  = sum_tc
                       , repr_con_tys = tys
                       , repr_cons    = cons })
      = do
          vars  <- newLocalVars (fsLit "x") tys
          es    <- zipWithM from_con (map Var vars) cons
          return $ mkWildCase expr (exprType expr) res_ty
                   [(DataAlt con, [var], e)
                      | (con, var, e) <- zip3 (tyConDataCons sum_tc) vars es]

    from_con expr (ConRepr con r)
      = from_prod expr (mkConApp con $ map Type ty_args) r

    from_prod _ con EmptyProd = return con
    from_prod expr con (UnaryProd r)
      = do
          e <- from_comp expr r
          return $ con `App` e
     
    from_prod expr con (Prod { repr_tup_tc   = tup_tc
                             , repr_comp_tys = tys
                             , repr_comps    = comps
                             })
      = do
          vars <- newLocalVars (fsLit "y") tys
          es   <- zipWithM from_comp (map Var vars) comps
          return $ mkWildCase expr (exprType expr) res_ty
                   [(DataAlt tup_con, vars, con `mkApps` es)]
      where
        [tup_con] = tyConDataCons tup_tc  

    from_comp expr (Keep _ _) = return expr
    from_comp expr (Wrap ty)
      = do
          wrap <- builtin wrapTyCon
          return $ unwrapNewTypeBody wrap [ty] expr


buildToArrPRepr :: TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr
buildToArrPRepr vect_tc prepr_tc pdata_tc r
  = do
      arg_ty <- mkPDataType el_ty
      res_ty <- mkPDataType =<< mkPReprType el_ty
      arg    <- newLocalVar (fsLit "xs") arg_ty

      pdata_co <- mkBuiltinCo pdataTyCon
      let Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co           = mkAppCo pdata_co
                       . mkSymCo
                       $ mkAxInstCo repr_co ty_args

          scrut   = unwrapFamInstScrut pdata_tc ty_args (Var arg)

      (vars, result) <- to_sum r

      return . Lam arg
             $ mkWildCase scrut (mkTyConApp pdata_tc ty_args) res_ty
               [(DataAlt pdata_dc, vars, mkCoerce co result)]
  where
    ty_args = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc ty_args

    [pdata_dc] = tyConDataCons pdata_tc


    to_sum EmptySum = do
                        pvoid <- builtin pvoidVar
                        return ([], Var pvoid)
    to_sum (UnarySum r) = to_con r
    to_sum (Sum { repr_psum_tc = psum_tc
                , repr_sel_ty  = sel_ty
                , repr_con_tys = tys
                , repr_cons    = cons
                })
      = do
          (vars, exprs) <- mapAndUnzipM to_con cons
          sel <- newLocalVar (fsLit "sel") sel_ty
          return (sel : concat vars, mk_result (Var sel) exprs)
      where
        [psum_con] = tyConDataCons psum_tc
        mk_result sel exprs = wrapFamInstBody psum_tc tys
                            $ mkConApp psum_con
                            $ map Type tys ++ (sel : exprs)

    to_con (ConRepr _ r) = to_prod r

    to_prod EmptyProd = do
                          pvoid <- builtin pvoidVar
                          return ([], Var pvoid)
    to_prod (UnaryProd r)
      = do
          pty  <- mkPDataType (compOrigType r)
          var  <- newLocalVar (fsLit "x") pty
          expr <- to_comp (Var var) r
          return ([var], expr)

    to_prod (Prod { repr_ptup_tc  = ptup_tc
                  , repr_comp_tys = tys
                  , repr_comps    = comps })
      = do
          ptys <- mapM (mkPDataType . compOrigType) comps
          vars <- newLocalVars (fsLit "x") ptys
          es   <- zipWithM to_comp (map Var vars) comps
          return (vars, mk_result es)
      where
        [ptup_con] = tyConDataCons ptup_tc
        mk_result exprs = wrapFamInstBody ptup_tc tys
                        $ mkConApp ptup_con
                        $ map Type tys ++ exprs

    to_comp expr (Keep _ _) = return expr

    -- FIXME: this is bound to be wrong!
    to_comp expr (Wrap ty)
      = do
          wrap_tc  <- builtin wrapTyCon
          (pwrap_tc, _) <- pdataReprTyCon (mkTyConApp wrap_tc [ty])
          return $ wrapNewTypeBody pwrap_tc [ty] expr


buildFromArrPRepr :: TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr
buildFromArrPRepr vect_tc prepr_tc pdata_tc r
  = do
      arg_ty <- mkPDataType =<< mkPReprType el_ty
      res_ty <- mkPDataType el_ty
      arg    <- newLocalVar (fsLit "xs") arg_ty

      pdata_co <- mkBuiltinCo pdataTyCon
      let Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co           = mkAppCo pdata_co
                       $ mkAxInstCo repr_co var_tys

          scrut  = mkCoerce co (Var arg)

          mk_result args = wrapFamInstBody pdata_tc var_tys
                         $ mkConApp pdata_con
                         $ map Type var_tys ++ args

      (expr, _) <- fixV $ \ ~(_, args) ->
                     from_sum res_ty (mk_result args) scrut r

      return $ Lam arg expr
    
      -- (args, mk) <- from_sum res_ty scrut r
      
      -- let result = wrapFamInstBody pdata_tc var_tys
      --           . mkConApp pdata_dc
      --           $ map Type var_tys ++ args

      -- return $ Lam arg (mk result)
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc var_tys

    [pdata_con] = tyConDataCons pdata_tc

    from_sum _ res _ EmptySum = return (res, [])
    from_sum res_ty res expr (UnarySum r) = from_con res_ty res expr r
    from_sum res_ty res expr (Sum { repr_psum_tc = psum_tc
                                  , repr_sel_ty  = sel_ty
                                  , repr_con_tys = tys
                                  , repr_cons    = cons })
      = do
          sel  <- newLocalVar (fsLit "sel") sel_ty
          ptys <- mapM mkPDataType tys
          vars <- newLocalVars (fsLit "xs") ptys
          (res', args) <- fold from_con res_ty res (map Var vars) cons
          let scrut = unwrapFamInstScrut psum_tc tys expr
              body  = mkWildCase scrut (exprType scrut) res_ty
                      [(DataAlt psum_con, sel : vars, res')]
          return (body, Var sel : args)
      where
        [psum_con] = tyConDataCons psum_tc


    from_con res_ty res expr (ConRepr _ r) = from_prod res_ty res expr r

    from_prod _ res _ EmptyProd = return (res, [])
    from_prod res_ty res expr (UnaryProd r)
      = from_comp res_ty res expr r
    from_prod res_ty res expr (Prod { repr_ptup_tc  = ptup_tc
                                    , repr_comp_tys = tys
                                    , repr_comps    = comps })
      = do
          ptys <- mapM mkPDataType tys
          vars <- newLocalVars (fsLit "ys") ptys
          (res', args) <- fold from_comp res_ty res (map Var vars) comps
          let scrut = unwrapFamInstScrut ptup_tc tys expr
              body  = mkWildCase scrut (exprType scrut) res_ty
                      [(DataAlt ptup_con, vars, res')]
          return (body, args)
      where
        [ptup_con] = tyConDataCons ptup_tc

    from_comp _ res expr (Keep _ _) = return (res, [expr])
    from_comp _ res expr (Wrap ty)
      = do
          wrap_tc  <- builtin wrapTyCon
          (pwrap_tc, _) <- pdataReprTyCon (mkTyConApp wrap_tc [ty])
          return (res, [unwrapNewTypeBody pwrap_tc [ty]
                        $ unwrapFamInstScrut pwrap_tc [ty] expr])

    fold f res_ty res exprs rs = foldrM f' (res, []) (zip exprs rs)
      where
        f' (expr, r) (res, args) = do
                                     (res', args') <- f res_ty res expr r
                                     return (res', args' ++ args)

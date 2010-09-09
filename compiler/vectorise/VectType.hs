{-# OPTIONS -fno-warn-missing-signatures #-}

module VectType ( vectTyCon, vectAndLiftType, vectType, vectTypeEnv,
                  -- arrSumArity, pdataCompTys, pdataCompVars,
                  buildPADict,
                  fromVect )
where

import VectUtils
import Vectorise.Env
import Vectorise.Convert
import Vectorise.Vect
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Type
import Vectorise.Type.TyConDecl
import Vectorise.Type.Classify
import Vectorise.Utils.Closure
import Vectorise.Utils.Hoisting

import HscTypes          ( TypeEnv, extendTypeEnvList, typeEnvTyCons )
import BasicTypes
import CoreSyn
import CoreUtils
import CoreUnfold
import MkCore		 ( mkWildCase )
import BuildTyCl
import DataCon
import TyCon
import Type
import Coercion
import FamInstEnv        ( FamInst, mkLocalFamInst )
import OccName
import Id
import MkId
import Var
import Name              ( Name, getOccName )
import NameEnv

import Unique
import UniqFM
import Util

import Outputable
import FastString

import MonadUtils     ( zipWith3M, foldrM, concatMapM )
import Control.Monad  ( liftM, liftM2, zipWithM, zipWithM_, mapAndUnzipM )
import Data.List

debug		= False
dtrace s x	= if debug then pprTrace "VectType" s x else x


-- | Vectorise a type environment.
--   The type environment contains all the type things defined in a module.
vectTypeEnv 
	:: TypeEnv
	-> VM ( TypeEnv			-- Vectorised type environment.
	      , [FamInst]		-- New type family instances.
	      , [(Var, CoreExpr)])	-- New top level bindings.
	
vectTypeEnv env
 = dtrace (ppr env)
 $ do
      cs <- readGEnv $ mk_map . global_tycons

      -- Split the list of TyCons into the ones we have to vectorise vs the
      -- ones we can pass through unchanged. We also pass through algebraic 
      -- types that use non Haskell98 features, as we don't handle those.
      let (conv_tcs, keep_tcs) = classifyTyCons cs groups
          keep_dcs             = concatMap tyConDataCons keep_tcs

      zipWithM_ defTyCon   keep_tcs keep_tcs
      zipWithM_ defDataCon keep_dcs keep_dcs

      new_tcs <- vectTyConDecls conv_tcs

      let orig_tcs = keep_tcs ++ conv_tcs

      -- We don't need to make new representation types for dictionary
      -- constructors. The constructors are always fully applied, and we don't 
      -- need to lift them to arrays as a dictionary of a particular type
      -- always has the same value.
      let vect_tcs = filter (not . isClassTyCon) 
                   $ keep_tcs ++ new_tcs

      (_, binds, inst_tcs) <- fixV $ \ ~(dfuns', _, _) ->
        do
          defTyConPAs (zipLazy vect_tcs dfuns')
          reprs     <- mapM tyConRepr vect_tcs
          repr_tcs  <- zipWith3M buildPReprTyCon orig_tcs vect_tcs reprs
          pdata_tcs <- zipWith3M buildPDataTyCon orig_tcs vect_tcs reprs

          dfuns     <- sequence 
                    $  zipWith5 buildTyConBindings
                               orig_tcs
                               vect_tcs
                               repr_tcs
                               pdata_tcs
                               reprs

          binds     <- takeHoisted
          return (dfuns, binds, repr_tcs ++ pdata_tcs)

      let all_new_tcs = new_tcs ++ inst_tcs

      let new_env = extendTypeEnvList env
                       (map ATyCon all_new_tcs
                        ++ [ADataCon dc | tc <- all_new_tcs
                                        , dc <- tyConDataCons tc])

      return (new_env, map mkLocalFamInst inst_tcs, binds)
  where
    tycons = typeEnvTyCons env
    groups = tyConGroups tycons

    mk_map env = listToUFM_Directly [(u, getUnique n /= u) | (u,n) <- nameEnvUniqueElts env]


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
                             (Just $ mk_fam_inst prepr_tc vect_tc)
  where
    tyvars = tyConTyVars vect_tc

data CompRepr = Keep Type
                     CoreExpr     -- PR dictionary for the type
              | Wrap Type

data ProdRepr = EmptyProd
              | UnaryProd CompRepr
              | Prod { repr_tup_tc   :: TyCon  -- representation tuple tycon
                     , repr_ptup_tc  :: TyCon  -- PData representation tycon
                     , repr_comp_tys :: [Type] -- representation types of
                     , repr_comps    :: [CompRepr]          -- components
                     }
data ConRepr  = ConRepr DataCon ProdRepr

data SumRepr  = EmptySum
              | UnarySum ConRepr
              | Sum  { repr_sum_tc   :: TyCon  -- representation sum tycon
                     , repr_psum_tc  :: TyCon  -- PData representation tycon
                     , repr_sel_ty   :: Type   -- type of selector
                     , repr_con_tys :: [Type]  -- representation types of
                     , repr_cons     :: [ConRepr]           -- components
                     }

tyConRepr :: TyCon -> VM SumRepr
tyConRepr tc = sum_repr (tyConDataCons tc)
  where
    sum_repr []    = return EmptySum
    sum_repr [con] = liftM UnarySum (con_repr con)
    sum_repr cons  = do
                       rs     <- mapM con_repr cons
                       sum_tc <- builtin (sumTyCon arity)
                       tys    <- mapM conReprType rs
                       (psum_tc, _) <- pdataReprTyCon (mkTyConApp sum_tc tys)
                       sel_ty <- builtin (selTy arity)
                       return $ Sum { repr_sum_tc  = sum_tc
                                    , repr_psum_tc = psum_tc
                                    , repr_sel_ty  = sel_ty
                                    , repr_con_tys = tys
                                    , repr_cons    = rs
                                    }
      where
        arity = length cons

    con_repr con = liftM (ConRepr con) (prod_repr (dataConRepArgTys con))

    prod_repr []   = return EmptyProd
    prod_repr [ty] = liftM UnaryProd (comp_repr ty)
    prod_repr tys  = do
                       rs <- mapM comp_repr tys
                       tup_tc <- builtin (prodTyCon arity)
                       tys'    <- mapM compReprType rs
                       (ptup_tc, _) <- pdataReprTyCon (mkTyConApp tup_tc tys')
                       return $ Prod { repr_tup_tc   = tup_tc
                                     , repr_ptup_tc  = ptup_tc
                                     , repr_comp_tys = tys'
                                     , repr_comps    = rs
                                     }
      where
        arity = length tys
    
    comp_repr ty = liftM (Keep ty) (prDictOfType ty)
                   `orElseV` return (Wrap ty)

sumReprType :: SumRepr -> VM Type
sumReprType EmptySum = voidType
sumReprType (UnarySum r) = conReprType r
sumReprType (Sum { repr_sum_tc  = sum_tc, repr_con_tys = tys })
  = return $ mkTyConApp sum_tc tys

conReprType :: ConRepr -> VM Type
conReprType (ConRepr _ r) = prodReprType r

prodReprType :: ProdRepr -> VM Type
prodReprType EmptyProd = voidType
prodReprType (UnaryProd r) = compReprType r
prodReprType (Prod { repr_tup_tc = tup_tc, repr_comp_tys = tys })
  = return $ mkTyConApp tup_tc tys

compReprType :: CompRepr -> VM Type
compReprType (Keep ty _) = return ty
compReprType (Wrap ty) = do
                             wrap_tc <- builtin wrapTyCon
                             return $ mkTyConApp wrap_tc [ty]

compOrigType :: CompRepr -> Type
compOrigType (Keep ty _) = ty
compOrigType (Wrap ty) = ty

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
          co           = mkAppCoercion pdata_co
                       . mkSymCoercion
                       $ mkTyConApp repr_co ty_args

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
          co           = mkAppCoercion pdata_co
                       $ mkTyConApp repr_co var_tys

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

buildPRDict :: TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr
buildPRDict vect_tc prepr_tc _ r
  = do
      dict <- sum_dict r
      pr_co <- mkBuiltinCo prTyCon
      let co = mkAppCoercion pr_co
             . mkSymCoercion
             $ mkTyConApp arg_co ty_args
      return (mkCoerce co dict)
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)
    Just arg_co = tyConFamilyCoercion_maybe prepr_tc

    sum_dict EmptySum = prDFunOfTyCon =<< builtin voidTyCon
    sum_dict (UnarySum r) = con_dict r
    sum_dict (Sum { repr_sum_tc  = sum_tc
                  , repr_con_tys = tys
                  , repr_cons    = cons
                  })
      = do
          dicts <- mapM con_dict cons
          dfun  <- prDFunOfTyCon sum_tc
          return $ dfun `mkTyApps` tys `mkApps` dicts

    con_dict (ConRepr _ r) = prod_dict r

    prod_dict EmptyProd = prDFunOfTyCon =<< builtin voidTyCon
    prod_dict (UnaryProd r) = comp_dict r
    prod_dict (Prod { repr_tup_tc   = tup_tc
                    , repr_comp_tys = tys
                    , repr_comps    = comps })
      = do
          dicts <- mapM comp_dict comps
          dfun <- prDFunOfTyCon tup_tc
          return $ dfun `mkTyApps` tys `mkApps` dicts

    comp_dict (Keep _ pr) = return pr
    comp_dict (Wrap ty)   = wrapPR ty


buildPDataTyCon :: TyCon -> TyCon -> SumRepr -> VM TyCon
buildPDataTyCon orig_tc vect_tc repr = fixV $ \repr_tc ->
  do
    name' <- cloneName mkPDataTyConOcc orig_name
    rhs   <- buildPDataTyConRhs orig_name vect_tc repr_tc repr
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


buildPDataTyConRhs :: Name -> TyCon -> TyCon -> SumRepr -> VM AlgTyConRhs
buildPDataTyConRhs orig_name vect_tc repr_tc repr
  = do
      data_con <- buildPDataDataCon orig_name vect_tc repr_tc repr
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPDataDataCon :: Name -> TyCon -> TyCon -> SumRepr -> VM DataCon
buildPDataDataCon orig_name vect_tc repr_tc repr
  = do
      dc_name  <- cloneName mkPDataDataConOcc orig_name
      comp_tys <- sum_tys repr

      liftDs $ buildDataCon dc_name
                            False                  -- not infix
                            (map (const HsNoBang) comp_tys)
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

    sum_tys EmptySum = return []
    sum_tys (UnarySum r) = con_tys r
    sum_tys (Sum { repr_sel_ty = sel_ty
                 , repr_cons   = cons })
      = liftM (sel_ty :) (concatMapM con_tys cons)

    con_tys (ConRepr _ r) = prod_tys r

    prod_tys EmptyProd = return []
    prod_tys (UnaryProd r) = liftM singleton (comp_ty r)
    prod_tys (Prod { repr_comps = comps }) = mapM comp_ty comps

    comp_ty r = mkPDataType (compOrigType r)


buildTyConBindings :: TyCon -> TyCon -> TyCon -> TyCon -> SumRepr 
                   -> VM Var
buildTyConBindings orig_tc vect_tc prepr_tc pdata_tc repr
  = do
      vectDataConWorkers orig_tc vect_tc pdata_tc
      buildPADict vect_tc prepr_tc pdata_tc repr

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
          arity <- polyArity tyvars
          body <- closedV
                . inBind orig_worker
                . polyAbstract tyvars $ \args ->
                  liftM (mkLams (tyvars ++ args) . vectorised)
                $ buildClosures tyvars [] arg_tys res_ty mk_body

          raw_worker <- cloneId mkVectOcc orig_worker (exprType body)
          let vect_worker = raw_worker `setIdUnfolding`
                              mkInlineRule body (Just arity)
          defGlobalVar orig_worker vect_worker
          return (vect_worker, body)
      where
        orig_worker = dataConWorkId data_con

buildPADict :: TyCon -> TyCon -> TyCon -> SumRepr -> VM Var
buildPADict vect_tc prepr_tc arr_tc repr
  = polyAbstract tvs $ \args ->
    do
      method_ids <- mapM (method args) paMethods

      pa_tc  <- builtin paTyCon
      pa_dc  <- builtin paDataCon
      let dict = mkLams (tvs ++ args)
               $ mkConApp pa_dc
               $ Type inst_ty : map (method_call args) method_ids

          dfun_ty = mkForAllTys tvs
                  $ mkFunTys (map varType args) (mkTyConApp pa_tc [inst_ty])

      raw_dfun <- newExportedVar dfun_name dfun_ty
      let dfun = raw_dfun `setIdUnfolding` mkDFunUnfolding dfun_ty (map Var method_ids)
                          `setInlinePragma` dfunInlinePragma

      hoistBinding dfun dict
      return dfun
  where
    tvs = tyConTyVars vect_tc
    arg_tys = mkTyVarTys tvs
    inst_ty = mkTyConApp vect_tc arg_tys

    dfun_name = mkPADFunOcc (getOccName vect_tc)

    method args (name, build)
      = localV
      $ do
          expr <- build vect_tc prepr_tc arr_tc repr
          let body = mkLams (tvs ++ args) expr
          raw_var <- newExportedVar (method_name name) (exprType body)
          let var = raw_var
                      `setIdUnfolding` mkInlineRule body (Just (length args))
                      `setInlinePragma` alwaysInlinePragma
          hoistBinding var body
          return var

    method_call args id = mkApps (Var id) (map Type arg_tys ++ map Var args)

    method_name name = mkVarOcc $ occNameString dfun_name ++ ('$' : name)


paMethods :: [(String, TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr)]
paMethods = [("dictPRepr",    buildPRDict),
             ("toPRepr",      buildToPRepr),
             ("fromPRepr",    buildFromPRepr),
             ("toArrPRepr",   buildToArrPRepr),
             ("fromArrPRepr", buildFromArrPRepr)]



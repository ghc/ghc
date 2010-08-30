{-# OPTIONS -fno-warn-missing-signatures #-}

module Vectorise( vectorise )
where

import VectMonad
import VectUtils
import VectType
import VectCore

import HscTypes hiding      ( MonadThings(..) )

import Module               ( PackageId )
import CoreSyn
import CoreUtils
import CoreUnfold           ( mkInlineRule )
import MkCore               ( mkWildCase )
import CoreFVs
import CoreMonad            ( CoreM, getHscEnv )
import DataCon
import TyCon
import Type
import FamInstEnv           ( extendFamInstEnvList )
import Var
import VarEnv
import VarSet
import Id
import OccName
import BasicTypes           ( isLoopBreaker )

import Literal              ( Literal, mkMachInt )
import TysWiredIn
import TysPrim              ( intPrimTy )

import Outputable
import FastString
import Util                 ( zipLazy )
import Control.Monad
import Data.List            ( sortBy, unzip4 )


debug		= False
dtrace s x	= if debug then pprTrace "Vectorise" s x else x

-- | Vectorise a single module.
--   Takes the package containing the DPH backend we're using. Eg either dph-par or dph-seq.
vectorise :: PackageId -> ModGuts -> CoreM ModGuts
vectorise backend guts 
 = do hsc_env <- getHscEnv
      liftIO $ vectoriseIO backend hsc_env guts


-- | Vectorise a single monad, given its HscEnv (code gen environment).
vectoriseIO :: PackageId -> HscEnv -> ModGuts -> IO ModGuts
vectoriseIO backend hsc_env guts
 = do -- Get information about currently loaded external packages.
      eps <- hscEPS hsc_env

      -- Combine vectorisation info from the current module, and external ones.
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps

      -- Run the main VM computation.
      Just (info', guts') <- initV backend hsc_env guts info (vectModule guts)
      return (guts' { mg_vect_info = info' })


-- | Vectorise a single module, in the VM monad.
vectModule :: ModGuts -> VM ModGuts
vectModule guts
 = do -- Vectorise the type environment.
      -- This may add new TyCons and DataCons.
      -- TODO: What new binds do we get back here?
      (types', fam_insts, tc_binds) <- vectTypeEnv (mg_types guts)

      -- TODO: What is this?
      let fam_inst_env' = extendFamInstEnvList (mg_fam_inst_env guts) fam_insts
      updGEnv (setFamInstEnv fam_inst_env')

      -- dicts   <- mapM buildPADict pa_insts
      -- workers <- mapM vectDataConWorkers pa_insts

      -- Vectorise all the top level bindings.
      binds'  <- mapM vectTopBind (mg_binds guts)

      return $ guts { mg_types        = types'
                    , mg_binds        = Rec tc_binds : binds'
                    , mg_fam_inst_env = fam_inst_env'
                    , mg_fam_insts    = mg_fam_insts guts ++ fam_insts
                    }


-- | Try to vectorise a top-level binding.
--   If it doesn't vectorise then return it unharmed.
--
--   For example, for the binding 
--
--   @  
--      foo :: Int -> Int
--      foo = \x -> x + x
--   @
--  
--   we get
--   @
--      foo  :: Int -> Int
--      foo  = \x -> vfoo $: x                  
-- 
--      v_foo :: Closure void vfoo lfoo
--      v_foo = closure vfoo lfoo void        
-- 
--      vfoo :: Void -> Int -> Int
--      vfoo = ...
--
--      lfoo :: PData Void -> PData Int -> PData Int
--      lfoo = ...
--   @ 
--
--   @vfoo@ is the "vectorised", or scalar, version that does the same as the original
--   function foo, but takes an explicit environment.
-- 
--   @lfoo@ is the "lifted" version that works on arrays.
--
--   @v_foo@ combines both of these into a `Closure` that also contains the
--   environment.
--
--   The original binding @foo@ is rewritten to call the vectorised version
--   present in the closure.
--
vectTopBind :: CoreBind -> VM CoreBind
vectTopBind b@(NonRec var expr)
 = do
      (inline, expr') 	<- vectTopRhs var expr
      var' 		<- vectTopBinder var inline expr'

      -- Vectorising the body may create other top-level bindings.
      hs	<- takeHoisted

      -- To get the same functionality as the original body we project
      -- out its vectorised version from the closure.
      cexpr	<- tryConvert var var' expr

      return . Rec $ (var, cexpr) : (var', expr') : hs
  `orElseV`
    return b

vectTopBind b@(Rec bs)
 = do
      (vars', _, exprs') 
	<- fixV $ \ ~(_, inlines, rhss) ->
            do vars' <- sequence [vectTopBinder var inline rhs
                                      | (var, ~(inline, rhs)) <- zipLazy vars (zip inlines rhss)]
               (inlines', exprs') 
                     <- mapAndUnzipM (uncurry vectTopRhs) bs

               return (vars', inlines', exprs')

      hs     <- takeHoisted
      cexprs <- sequence $ zipWith3 tryConvert vars vars' exprs
      return . Rec $ zip vars cexprs ++ zip vars' exprs' ++ hs
  `orElseV`
    return b
  where
    (vars, exprs) = unzip bs


-- | Make the vectorised version of this top level binder, and add the mapping
--   between it and the original to the state. For some binder @foo@ the vectorised
--   version is @$v_foo@
--
--   NOTE: vectTopBinder *MUST* be lazy in inline and expr because of how it is
--   used inside of fixV in vectTopBind
vectTopBinder 
	:: Var 		-- ^ Name of the binding.
	-> Inline 	-- ^ Whether it should be inlined, used to annotate it.
	-> CoreExpr 	-- ^ RHS of the binding, used to set the `Unfolding` of the returned `Var`.
	-> VM Var	-- ^ Name of the vectorised binding.

vectTopBinder var inline expr
 = do
      -- Vectorise the type attached to the var.
      vty  <- vectType (idType var)

      -- Make the vectorised version of binding's name, and set the unfolding used for inlining.
      var' <- liftM (`setIdUnfolding` unfolding) 
           $  cloneId mkVectOcc var vty

      -- Add the mapping between the plain and vectorised name to the state.
      defGlobalVar var var'

      return var'
  where
    unfolding = case inline of
                  Inline arity -> mkInlineRule expr (Just arity)
                  DontInline   -> noUnfolding


-- | Vectorise the RHS of a top-level binding, in an empty local environment.
vectTopRhs 
	:: Var 		-- ^ Name of the binding.
	-> CoreExpr	-- ^ Body of the binding.
	-> VM (Inline, CoreExpr)

vectTopRhs var expr
 = dtrace (vcat [text "vectTopRhs", ppr expr])
 $ closedV
 $ do (inline, vexpr) <- inBind var
                      $ vectPolyExpr (isLoopBreaker $ idOccInfo var)
                                      (freeVars expr)
      return (inline, vectorised vexpr)


-- | Project out the vectorised version of a binding from some closure,
--	or return the original body if that doesn't work.	
tryConvert 
	:: Var	 	-- ^ Name of the original binding (eg @foo@)
	-> Var 		-- ^ Name of vectorised version of binding (eg @$vfoo@)
	-> CoreExpr	-- ^ The original body of the binding.
	-> VM CoreExpr

tryConvert var vect_var rhs
  = fromVect (idType var) (Var vect_var) `orElseV` return rhs


-- ----------------------------------------------------------------------------
-- Bindings

-- | Vectorise a binder variable, along with its attached type.
vectBndr :: Var -> VM VVar
vectBndr v
  = do
      (vty, lty) <- vectAndLiftType (idType v)
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty
      updLEnv (mapTo vv lv)
      return (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (vv, lv) }


-- | Vectorise a binder variable, along with its attached type, 
--   but give the result a new name.
vectBndrNew :: Var -> FastString -> VM VVar
vectBndrNew v fs
  = do
      vty <- vectType (idType v)
      vv  <- newLocalVVar fs vty
      updLEnv (upd vv)
      return vv
  where
    upd vv env = env { local_vars = extendVarEnv (local_vars env) v vv }


-- | Vectorise a binder then run a computation with that binder in scope.
vectBndrIn :: Var -> VM a -> VM (VVar, a)
vectBndrIn v p
  = localV
  $ do
      vv <- vectBndr v
      x <- p
      return (vv, x)


-- | Vectorise a binder, give it a new name, then run a computation with that binder in scope.
vectBndrNewIn :: Var -> FastString -> VM a -> VM (VVar, a)
vectBndrNewIn v fs p
  = localV
  $ do
      vv <- vectBndrNew v fs
      x  <- p
      return (vv, x)

-- | Vectorise some binders, then run a computation with them in scope.
vectBndrsIn :: [Var] -> VM a -> VM ([VVar], a)
vectBndrsIn vs p
  = localV
  $ do
      vvs <- mapM vectBndr vs
      x <- p
      return (vvs, x)


-- ----------------------------------------------------------------------------
-- Expressions

-- | Vectorise a variable, producing the vectorised and lifted versions.
vectVar :: Var -> VM VExpr
vectVar v
 = do 
      -- lookup the variable from the environment.
      r <- lookupVar v

      case r of
        -- If it's been locally bound then we'll already have both versions available.
        Local (vv,lv) 
         -> return (Var vv, Var lv)

        -- To create the lifted version of a global variable we replicate it.
        Global vv     
         -> do let vexpr = Var vv
               lexpr <- liftPD vexpr
               return (vexpr, lexpr)


-- | Like `vectVar` but also add type applications to the variables.
vectPolyVar :: Var -> [Type] -> VM VExpr
vectPolyVar v tys
  = do
      vtys	<- mapM vectType tys
      r		<- lookupVar v
      case r of
        Local (vv, lv) 
         -> liftM2 (,) (polyApply (Var vv) vtys)
                       (polyApply (Var lv) vtys)

        Global poly    
         -> do vexpr <- polyApply (Var poly) vtys
               lexpr <- liftPD vexpr
               return (vexpr, lexpr)


-- | Lifted literals are created by replicating them.
vectLiteral :: Literal -> VM VExpr
vectLiteral lit
  = do
      lexpr <- liftPD (Lit lit)
      return (Lit lit, lexpr)


-- | Vectorise a polymorphic expression
vectPolyExpr 
	:: Bool 		-- ^ When vectorising the RHS of a binding, whether that
				--   binding is a loop breaker.
	-> CoreExprWithFVs
	-> VM (Inline, VExpr)

vectPolyExpr loop_breaker (_, AnnNote note expr)
 = do (inline, expr') <- vectPolyExpr loop_breaker expr
      return (inline, vNote note expr')

vectPolyExpr loop_breaker expr
 = dtrace (vcat [text "vectPolyExpr", ppr (deAnnotate expr)])
 $ do
      arity <- polyArity tvs
      polyAbstract tvs $ \args ->
        do
          (inline, mono') <- vectFnExpr False loop_breaker mono
          return (addInlineArity inline arity,
                  mapVect (mkLams $ tvs ++ args) mono')
  where
    (tvs, mono) = collectAnnTypeBinders expr


-- | Vectorise a core expression.
vectExpr :: CoreExprWithFVs -> VM VExpr
vectExpr (_, AnnType ty)
  = liftM vType (vectType ty)

vectExpr (_, AnnVar v) 
  = vectVar v

vectExpr (_, AnnLit lit) 
  = vectLiteral lit

vectExpr (_, AnnNote note expr)
  = liftM (vNote note) (vectExpr expr)

vectExpr e@(_, AnnApp _ arg)
  | isAnnTypeArg arg
  = vectTyAppExpr fn tys
  where
    (fn, tys) = collectAnnTypeArgs e

vectExpr (_, AnnApp (_, AnnVar v) (_, AnnLit lit))
  | Just con <- isDataConId_maybe v
  , is_special_con con
  = do
      let vexpr = App (Var v) (Lit lit)
      lexpr <- liftPD vexpr
      return (vexpr, lexpr)
  where
    is_special_con con = con `elem` [intDataCon, floatDataCon, doubleDataCon]


-- TODO: Avoid using closure application for dictionaries.
-- vectExpr (_, AnnApp fn arg)
--  | if is application of dictionary 
--    just use regular app instead of closure app.

-- for lifted version. 
--      do liftPD (sub a dNumber)
--      lift the result of the selection, not sub and dNumber seprately. 

vectExpr (_, AnnApp fn arg)
 = dtrace (text "AnnApp" <+> ppr (deAnnotate fn) <+> ppr (deAnnotate arg))
 $ do
      arg_ty' <- vectType arg_ty
      res_ty' <- vectType res_ty

      dtrace (text "vectorising fn " <> ppr (deAnnotate fn))  $ return ()
      fn'     <- vectExpr fn
      dtrace (text "fn' = "       <> ppr fn') $ return ()

      arg'    <- vectExpr arg

      mkClosureApp arg_ty' res_ty' fn' arg'
  where
    (arg_ty, res_ty) = splitFunTy . exprType $ deAnnotate fn

vectExpr (_, AnnCase scrut bndr ty alts)
  | Just (tycon, ty_args) <- splitTyConApp_maybe scrut_ty
  , isAlgTyCon tycon
  = vectAlgCase tycon ty_args scrut bndr ty alts
  where
    scrut_ty = exprType (deAnnotate scrut)

vectExpr (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      vrhs <- localV . inBind bndr . liftM snd $ vectPolyExpr False rhs
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vLet (vNonRec vbndr vrhs) vbody

vectExpr (_, AnnLet (AnnRec bs) body)
  = do
      (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs
                                $ liftM2 (,)
                                  (zipWithM vect_rhs bndrs rhss)
                                  (vectExpr body)
      return $ vLet (vRec vbndrs vrhss) vbody
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs = localV
                      . inBind bndr
                      . liftM snd
                      $ vectPolyExpr (isLoopBreaker $ idOccInfo bndr) rhs

vectExpr e@(_, AnnLam bndr _)
  | isId bndr = liftM snd $ vectFnExpr True False e
{-
onlyIfV (isEmptyVarSet fvs) (vectScalarLam bs $ deAnnotate body)
                `orElseV` vectLam True fvs bs body
  where
    (bs,body) = collectAnnValBinders e
-}

vectExpr e = cantVectorise "Can't vectorise expression" (ppr $ deAnnotate e)


-- | Vectorise an expression with an outer lambda abstraction.
vectFnExpr 
	:: Bool 		-- ^ When the RHS of a binding, whether that binding should be inlined.
	-> Bool 		-- ^ Whether the binding is a loop breaker.
	-> CoreExprWithFVs 	-- ^ Expression to vectorise. Must have an outer `AnnLam`.
	-> VM (Inline, VExpr)

vectFnExpr inline loop_breaker e@(fvs, AnnLam bndr _)
  | isId bndr = onlyIfV (isEmptyVarSet fvs)
                        (mark DontInline . vectScalarLam bs $ deAnnotate body)
                `orElseV` mark inlineMe (vectLam inline loop_breaker fvs bs body)
  where
    (bs,body) = collectAnnValBinders e

vectFnExpr _ _ e = mark DontInline $ vectExpr e

mark :: Inline -> VM a -> VM (Inline, a)
mark b p = do { x <- p; return (b,x) }


-- | Vectorise a function where are the args have scalar type, that is Int, Float or Double.
vectScalarLam 
	:: [Var]	-- ^ Bound variables of function.
	-> CoreExpr	-- ^ Function body.
	-> VM VExpr
vectScalarLam args body
 = dtrace (vcat [text "vectScalarLam ", ppr args, ppr body])
 $ do scalars <- globalScalars
      onlyIfV (all is_scalar_ty arg_tys
               && is_scalar_ty res_ty
               && is_scalar (extendVarSetList scalars args) body
               && uses scalars body)
        $ do
            fn_var  <- hoistExpr (fsLit "fn") (mkLams args body) DontInline
            zipf    <- zipScalars arg_tys res_ty
            clo     <- scalarClosure arg_tys res_ty (Var fn_var)
                                                (zipf `App` Var fn_var)
            clo_var <- hoistExpr (fsLit "clo") clo DontInline
            lclo    <- liftPD (Var clo_var)
            return (Var clo_var, lclo)
  where
    arg_tys = map idType args
    res_ty  = exprType body

    is_scalar_ty ty 
        | Just (tycon, [])   <- splitTyConApp_maybe ty
        =    tycon == intTyCon
          || tycon == floatTyCon
          || tycon == doubleTyCon

        | otherwise = False

    is_scalar vs (Var v)     = v `elemVarSet` vs
    is_scalar _ e@(Lit _)    = is_scalar_ty $ exprType e
    is_scalar vs (App e1 e2) = is_scalar vs e1 && is_scalar vs e2
    is_scalar _ _            = False

    -- A scalar function has to actually compute something. Without the check,
    -- we would treat (\(x :: Int) -> x) as a scalar function and lift it to
    -- (map (\x -> x)) which is very bad. Normal lifting transforms it to
    -- (\n# x -> x) which is what we want.
    uses funs (Var v)     = v `elemVarSet` funs 
    uses funs (App e1 e2) = uses funs e1 || uses funs e2
    uses _ _              = False


vectLam 
	:: Bool			-- ^ When the RHS of a binding, whether that binding should be inlined.
	-> Bool			-- ^ Whether the binding is a loop breaker.
	-> VarSet		-- ^ The free variables in the body.
	-> [Var]		-- 
	-> CoreExprWithFVs
	-> VM VExpr

vectLam inline loop_breaker fvs bs body
 = dtrace (vcat [ text "vectLam "
		, text "free vars    = " <> ppr fvs
		, text "binding vars = " <> ppr bs
		, text "body         = " <> ppr (deAnnotate body)])

 $ do tyvars    <- localTyVars
      (vs, vvs) <- readLEnv $ \env ->
                   unzip [(var, vv) | var <- varSetElems fvs
                                    , Just vv <- [lookupVarEnv (local_vars env) var]]

      arg_tys   <- mapM (vectType . idType) bs

      dtrace (text "arg_tys = " <> ppr arg_tys) $ return ()

      res_ty    <- vectType (exprType $ deAnnotate body)

      dtrace (text "res_ty = " <> ppr res_ty) $ return ()

      buildClosures tyvars vvs arg_tys res_ty
        . hoistPolyVExpr tyvars (maybe_inline (length vs + length bs))
        $ do
            lc              <- builtin liftingContext
            (vbndrs, vbody) <- vectBndrsIn (vs ++ bs) (vectExpr body)

            dtrace (text "vbody = " <> ppr vbody) $ return ()

            vbody' <- break_loop lc res_ty vbody
            return $ vLams lc vbndrs vbody'
  where
    maybe_inline n | inline    = Inline n
                   | otherwise = DontInline

    break_loop lc ty (ve, le)
      | loop_breaker
      = do
          empty <- emptyPD ty
          lty <- mkPDataType ty
          return (ve, mkWildCase (Var lc) intPrimTy lty
                        [(DEFAULT, [], le),
                         (LitAlt (mkMachInt 0), [], empty)])

      | otherwise = return (ve, le)
 

vectTyAppExpr :: CoreExprWithFVs -> [Type] -> VM VExpr
vectTyAppExpr (_, AnnVar v) tys = vectPolyVar v tys
vectTyAppExpr e tys = cantVectorise "Can't vectorise expression"
                        (ppr $ deAnnotate e `mkTyApps` tys)

-- We convert
--
--   case e :: t of v { ... }
--
-- to
--
--   V:    let v' = e in case v' of _ { ... }
--   L:    let v' = e in case v' `cast` ... of _ { ... }
--
-- When lifting, we have to do it this way because v must have the type
-- [:V(T):] but the scrutinee must be cast to the representation type. We also
-- have to handle the case where v is a wild var correctly.
--

-- FIXME: this is too lazy
vectAlgCase :: TyCon -> [Type] -> CoreExprWithFVs -> Var -> Type
            -> [(AltCon, [Var], CoreExprWithFVs)]
            -> VM VExpr
vectAlgCase _tycon _ty_args scrut bndr ty [(DEFAULT, [], body)]
  = do
      vscrut         <- vectExpr scrut
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt _, [], body)]
  = do
      vscrut         <- vectExpr scrut
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt dc, bndrs, body)]
  = do
      (vty, lty) <- vectAndLiftType ty
      vexpr      <- vectExpr scrut
      (vbndr, (vbndrs, (vect_body, lift_body)))
         <- vect_scrut_bndr
          . vectBndrsIn bndrs
          $ vectExpr body
      let (vect_bndrs, lift_bndrs) = unzip vbndrs
      (vscrut, lscrut, pdata_tc, _arg_tys) <- mkVScrut (vVar vbndr)
      vect_dc <- maybeV (lookupDataCon dc)
      let [pdata_dc] = tyConDataCons pdata_tc

      let vcase = mk_wild_case vscrut vty vect_dc  vect_bndrs vect_body
          lcase = mk_wild_case lscrut lty pdata_dc lift_bndrs lift_body

      return $ vLet (vNonRec vbndr vexpr) (vcase, lcase)
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    mk_wild_case expr ty dc bndrs body
      = mkWildCase expr (exprType expr) ty [(DataAlt dc, bndrs, body)]

vectAlgCase tycon _ty_args scrut bndr ty alts
  = do
      vect_tc     <- maybeV (lookupTyCon tycon)
      (vty, lty)  <- vectAndLiftType ty

      let arity = length (tyConDataCons vect_tc)
      sel_ty <- builtin (selTy arity)
      sel_bndr <- newLocalVar (fsLit "sel") sel_ty
      let sel = Var sel_bndr

      (vbndr, valts) <- vect_scrut_bndr
                      $ mapM (proc_alt arity sel vty lty) alts'
      let (vect_dcs, vect_bndrss, lift_bndrss, vbodies) = unzip4 valts

      vexpr <- vectExpr scrut
      (vect_scrut, lift_scrut, pdata_tc, _arg_tys) <- mkVScrut (vVar vbndr)
      let [pdata_dc] = tyConDataCons pdata_tc

      let (vect_bodies, lift_bodies) = unzip vbodies

      vdummy <- newDummyVar (exprType vect_scrut)
      ldummy <- newDummyVar (exprType lift_scrut)
      let vect_case = Case vect_scrut vdummy vty
                           (zipWith3 mk_vect_alt vect_dcs vect_bndrss vect_bodies)

      lc <- builtin liftingContext
      lbody <- combinePD vty (Var lc) sel lift_bodies
      let lift_case = Case lift_scrut ldummy lty
                           [(DataAlt pdata_dc, sel_bndr : concat lift_bndrss,
                             lbody)]

      return . vLet (vNonRec vbndr vexpr)
             $ (vect_case, lift_case)
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    alts' = sortBy (\(alt1, _, _) (alt2, _, _) -> cmp alt1 alt2) alts

    cmp (DataAlt dc1) (DataAlt dc2) = dataConTag dc1 `compare` dataConTag dc2
    cmp DEFAULT       DEFAULT       = EQ
    cmp DEFAULT       _             = LT
    cmp _             DEFAULT       = GT
    cmp _             _             = panic "vectAlgCase/cmp"

    proc_alt arity sel _ lty (DataAlt dc, bndrs, body)
      = do
          vect_dc <- maybeV (lookupDataCon dc)
          let ntag = dataConTagZ vect_dc
              tag  = mkDataConTag vect_dc
              fvs  = freeVarsOf body `delVarSetList` bndrs

          sel_tags  <- liftM (`App` sel) (builtin (selTags arity))
          lc        <- builtin liftingContext
          elems     <- builtin (selElements arity ntag)

          (vbndrs, vbody)
            <- vectBndrsIn bndrs
             . localV
             $ do
                 binds    <- mapM (pack_var (Var lc) sel_tags tag)
                           . filter isLocalId
                           $ varSetElems fvs
                 (ve, le) <- vectExpr body
                 return (ve, Case (elems `App` sel) lc lty
                             [(DEFAULT, [], (mkLets (concat binds) le))])
                 -- empty    <- emptyPD vty
                 -- return (ve, Case (elems `App` sel) lc lty
                 --             [(DEFAULT, [], Let (NonRec flags_var flags_expr)
                 --                             $ mkLets (concat binds) le),
                 --               (LitAlt (mkMachInt 0), [], empty)])
          let (vect_bndrs, lift_bndrs) = unzip vbndrs
          return (vect_dc, vect_bndrs, lift_bndrs, vbody)

    proc_alt _ _ _ _ _ = panic "vectAlgCase/proc_alt"

    mk_vect_alt vect_dc bndrs body = (DataAlt vect_dc, bndrs, body)

    pack_var len tags t v
      = do
          r <- lookupVar v
          case r of
            Local (vv, lv) ->
              do
                lv'  <- cloneVar lv
                expr <- packByTagPD (idType vv) (Var lv) len tags t
                updLEnv (\env -> env { local_vars = extendVarEnv
                                                (local_vars env) v (vv, lv') })
                return [(NonRec lv' expr)]

            _ -> return []


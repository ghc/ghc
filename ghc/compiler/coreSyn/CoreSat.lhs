%
% (c) The University of Glasgow, 1994-2000
%
\section{Core pass to saturate constructors and PrimOps}

\begin{code}
module CoreSat (
      coreSatPgm, coreSatExpr
  ) where

#include "HsVersions.h"

import CoreUtils
import CoreFVs
import CoreLint
import CoreSyn
import Type
import Demand
import Var 	( TyVar, setTyVarUnique )
import VarSet
import IdInfo
import Id
import PrimOp
import UniqSupply
import Maybes
import ErrUtils
import CmdLineOpts
import Outputable
\end{code}

-- ---------------------------------------------------------------------------
-- Overview
-- ---------------------------------------------------------------------------

Most of the contents of this pass used to be in CoreToStg.  The
primary goals here are:

1.  Get the program into "A-normal form". In particular:

	f E	   ==>  let x = E in f x
		OR ==>  case E of x -> f x


    if E is a non-trivial expression.
    Which transformation is used depends on whether f is strict or not.
    [Previously the transformation to case used to be done by the
     simplifier, but it's better done here.  It does mean that f needs
     to have its strictness info correct!.]

2.  Similarly, convert any unboxed lets into cases.
    [I'm experimenting with leaving 'ok-for-speculation' rhss in let-form
     right up to this point.]

    This is all done modulo type applications and abstractions, so that
    when type erasure is done for conversion to STG, we don't end up with
    any trivial or useless bindings.
  
3.  Ensure that lambdas only occur as the RHS of a binding
    (The code generator can't deal with anything else.)

4.  Saturate constructor and primop applications.



-- -----------------------------------------------------------------------------
-- Top level stuff
-- -----------------------------------------------------------------------------

\begin{code}
coreSatPgm :: DynFlags -> [CoreBind] -> IO [CoreBind]
coreSatPgm dflags binds 
  = do	showPass dflags "CoreSat"
	us <- mkSplitUniqSupply 's'
	let new_binds = initUs_ us (coreSatBinds binds)
        endPass dflags "CoreSat" Opt_D_dump_sat new_binds

coreSatExpr :: DynFlags -> CoreExpr -> IO CoreExpr
coreSatExpr dflags expr
  = do showPass dflags "CoreSat"
       us <- mkSplitUniqSupply 's'
       let new_expr = initUs_ us (coreSatAnExpr expr)
       dumpIfSet_dyn dflags Opt_D_dump_sat "Saturated/Normal form syntax:" 
	  (ppr new_expr)
       return new_expr

-- ---------------------------------------------------------------------------
-- Dealing with bindings
-- ---------------------------------------------------------------------------

data FloatingBind
   = RecF [(Id, CoreExpr)]
   | NonRecF Id
	     CoreExpr		-- *Can* be a Lam
	     RhsDemand
	     [FloatingBind]

coreSatBinds :: [CoreBind] -> UniqSM [CoreBind]
coreSatBinds [] = returnUs []
coreSatBinds (b:bs)
  = coreSatBind b	`thenUs` \ float ->
    coreSatBinds bs 	`thenUs` \ new_bs ->
    case float of
	NonRecF bndr rhs dem floats 
		-> ASSERT2( not (isStrictDem dem) && 
			    not (isUnLiftedType (idType bndr)),
			    ppr b )		-- No top-level cases!

		   mkBinds floats rhs		`thenUs` \ new_rhs ->
		   returnUs (NonRec bndr new_rhs : new_bs)
				-- Keep all the floats inside...
				-- Some might be cases etc
				-- We might want to revisit this decision

	RecF prs -> returnUs (Rec prs : new_bs)

coreSatBind :: CoreBind -> UniqSM FloatingBind
coreSatBind (NonRec binder rhs)
  = coreSatExprFloat rhs		`thenUs` \ (floats, new_rhs) ->
    returnUs (NonRecF binder new_rhs (bdrDem binder) floats)
coreSatBind (Rec pairs)
  = mapUs do_rhs pairs 			`thenUs` \ new_rhss ->
    returnUs (RecF (binders `zip` new_rhss))
  where
    binders = map fst pairs
    do_rhs (bndr,rhs) = 
	coreSatExprFloat rhs		`thenUs` \ (floats, new_rhs) ->
	mkBinds floats new_rhs		`thenUs` \ new_rhs' ->
		-- NB: new_rhs' might still be a Lam (and we want that)
	returnUs new_rhs'

-- ---------------------------------------------------------------------------
-- Making arguments atomic (function args & constructor args)
-- ---------------------------------------------------------------------------

-- This is where we arrange that a non-trivial argument is let-bound
coreSatArg :: CoreArg -> RhsDemand -> UniqSM ([FloatingBind], CoreArg)
coreSatArg arg dem
  = coreSatExprFloat arg		`thenUs` \ (floats, arg') ->
    if exprIsTrivial arg'
	then returnUs (floats, arg')
	else newVar (exprType arg')	`thenUs` \ v ->
	     returnUs ([NonRecF v arg' dem floats], Var v)

-- ---------------------------------------------------------------------------
-- Dealing with expressions
-- ---------------------------------------------------------------------------

coreSatAnExpr :: CoreExpr -> UniqSM CoreExpr
coreSatAnExpr expr
  = coreSatExprFloat expr		`thenUs` \ (floats, expr) ->
    mkBinds floats expr


coreSatExprFloat :: CoreExpr -> UniqSM ([FloatingBind], CoreExpr)
-- If
--	e  ===>  (bs, e')
-- then	
--	e = let bs in e'	(semantically, that is!)
--
-- For example
--	f (g x)	  ===>   ([v = g x], f v)

coreSatExprFloat (Var v)
  = maybeSaturate v (Var v) 0 (idType v) `thenUs` \ app ->
    returnUs ([], app)

coreSatExprFloat (Lit lit)
  = returnUs ([], Lit lit)

coreSatExprFloat (Let bind body)
  = coreSatBind bind			`thenUs` \ new_bind ->
    coreSatExprFloat body		`thenUs` \ (floats, new_body) ->
    returnUs (new_bind:floats, new_body)

coreSatExprFloat (Note other_note expr)
  = coreSatExprFloat expr		`thenUs` \ (floats, expr) ->
    returnUs (floats, Note other_note expr)

coreSatExprFloat expr@(Type _)
  = returnUs ([], expr)

coreSatExprFloat (Lam v e)
  = coreSatAnExpr e			`thenUs` \ e' ->
    returnUs ([], Lam v e')

coreSatExprFloat (Case scrut bndr alts)
  = coreSatExprFloat scrut		`thenUs` \ (floats, scrut) ->
    mapUs sat_alt alts			`thenUs` \ alts ->
    returnUs (floats, Case scrut bndr alts)
  where
    sat_alt (con, bs, rhs)
	  = coreSatAnExpr rhs	  	 `thenUs` \ rhs ->
	    deLam rhs			 `thenUs` \ rhs ->
	    returnUs (con, bs, rhs)

coreSatExprFloat expr@(App _ _)
  = collect_args expr 0  `thenUs` \ (app,(head,depth),ty,floats,ss) ->
    ASSERT(null ss)	-- make sure we used all the strictness info

	-- Now deal with the function
    case head of
      Var fn_id -> maybeSaturate fn_id app depth ty `thenUs` \ app' -> 
	    	   returnUs (floats, app')

      _other    -> returnUs (floats, app)

  where

    -- Deconstruct and rebuild the application, floating any non-atomic
    -- arguments to the outside.  We collect the type of the expression,
    -- the head of the applicaiton, and the number of actual value arguments,
    -- all of which are used to possibly saturate this application if it
    -- has a constructor or primop at the head.

    collect_args
	:: CoreExpr
	-> Int				-- current app depth
	-> UniqSM (CoreExpr,		-- the rebuilt expression
		   (CoreExpr,Int),	-- the head of the application,
				          -- and no. of args it was applied to
		   Type,		-- type of the whole expr
		   [FloatingBind],	-- any floats we pulled out
		   [Demand])		-- remaining argument demands

    collect_args (App fun arg@(Type arg_ty)) depth
        = collect_args fun depth   `thenUs` \ (fun',hd,fun_ty,floats,ss) ->
	  returnUs (App fun' arg, hd, applyTy fun_ty arg_ty, floats, ss)

    collect_args (App fun arg) depth
        = collect_args fun (depth+1)   `thenUs` \ (fun',hd,fun_ty,floats,ss) ->
	  let
	      (ss1, ss_rest)   = case ss of
				   (ss1:ss_rest) -> (ss1, ss_rest)
				   []	       -> (wwLazy, [])
              (arg_ty, res_ty) = expectJust "coreSatExprFloat:collect_args" $
                                 splitFunTy_maybe fun_ty
	  in
	  coreSatArg arg (mkDemTy ss1 arg_ty) `thenUs` \ (fs, arg') ->
	  returnUs (App fun' arg', hd, res_ty, fs ++ floats, ss_rest)

    collect_args (Var v) depth
	= returnUs (Var v, (Var v, depth), idType v, [], stricts)
	where
	  stricts = case idStrictness v of
			StrictnessInfo demands _ 
			    | depth >= length demands -> demands
			    | otherwise               -> []
			other			      -> []
		-- If depth < length demands, then we have too few args to 
		-- satisfy strictness  info so we have to  ignore all the 
		-- strictness info, e.g. + (error "urk")
		-- Here, we can't evaluate the arg  strictly, because this 
		-- partial  application might be seq'd

    collect_args (Note (Coerce ty1 ty2) fun) depth
        = collect_args fun depth  `thenUs` \ (fun', hd, fun_ty, floats, ss) ->
	  returnUs (Note (Coerce ty1 ty2) fun', hd, ty1, floats, ss)

    collect_args (Note note fun) depth
	| ignore_note note 
        = collect_args fun depth   `thenUs` \ (fun', hd, fun_ty, floats, ss) ->
	  returnUs (Note note fun', hd, fun_ty, floats, ss)

	-- non-variable fun, better let-bind it
    collect_args fun depth
	= newVar ty		 	`thenUs` \ fn_id ->
          coreSatExprFloat fun	  	`thenUs` \ (fun_floats, fun) ->
	  returnUs (Var fn_id, (Var fn_id, depth), ty, 
		    [NonRecF fn_id fun onceDem fun_floats], [])
        where ty = exprType fun

    ignore_note	InlineCall = True
    ignore_note	InlineMe   = True
    ignore_note	_other     = False
	-- we don't ignore SCCs, since they require some code generation

------------------------------------------------------------------------------
-- Generating new binders
-- ---------------------------------------------------------------------------

newVar :: Type -> UniqSM Id
newVar ty
 = getUniqueUs	 		`thenUs` \ uniq ->
   seqType ty			`seq`
   returnUs (mkSysLocal SLIT("sat") uniq ty)

cloneTyVar :: TyVar -> UniqSM TyVar
cloneTyVar tv
 = getUniqueUs	 		`thenUs` \ uniq ->
   returnUs (setTyVarUnique tv uniq)

------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

-- maybeSaturate deals with saturating primops and constructors
-- The type is the type of the entire application
maybeSaturate :: Id -> CoreExpr -> Int -> Type -> UniqSM CoreExpr
maybeSaturate fn expr n_args ty
  = case idFlavour fn of
      PrimOpId op  -> saturate_it
      DataConId dc -> saturate_it
      other 	   -> returnUs expr
  where
    fn_arity	 = idArity fn
    excess_arity = fn_arity - n_args
    saturate_it  = getUs 	`thenUs` \ us ->
		   returnUs (etaExpand excess_arity us expr ty)

-- ---------------------------------------------------------------------------
-- Eliminate Lam as a non-rhs (STG doesn't have such a thing)
-- ---------------------------------------------------------------------------

deLam (Note n e)
  = deLam e `thenUs` \ e ->
    returnUs (Note n e)

   -- types will all disappear, so that's ok
deLam (Lam x e) | isTyVar x
  = deLam e `thenUs` \ e ->
    returnUs (Lam x e)

deLam expr@(Lam _ _) 
	-- Try for eta reduction
  | Just e <- eta body
  = returnUs e		

	-- Eta failed, so let-bind the lambda
  | otherwise
  = newVar (exprType expr) `thenUs` \ fn ->
    returnUs (Let (NonRec fn expr) (Var fn))

  where
    (bndrs, body) = collectBinders expr

    eta expr@(App _ _)
	| n_remaining >= 0 &&
	  and (zipWith ok bndrs last_args) &&
	  not (any (`elemVarSet` fvs_remaining) bndrs)
	= Just remaining_expr
	where
	  (f, args) = collectArgs expr
	  remaining_expr = mkApps f remaining_args
	  fvs_remaining = exprFreeVars remaining_expr
	  (remaining_args, last_args) = splitAt n_remaining args
	  n_remaining = length args - length bndrs

	  ok bndr (Var arg) = bndr == arg
	  ok bndr other	    = False

    eta (Let bind@(NonRec b r) body)
	| not (any (`elemVarSet` fvs) bndrs)
		 = case eta body of
			Just e -> Just (Let bind e)
			Nothing -> Nothing
	where fvs = exprFreeVars r

    eta _ = Nothing

deLam expr = returnUs expr

-- ---------------------------------------------------------------------------
-- Precipitating the floating bindings
-- ---------------------------------------------------------------------------

mkBinds :: [FloatingBind] -> CoreExpr -> UniqSM CoreExpr
mkBinds []     body = returnUs body
mkBinds (b:bs) body 
  = deLam body   	`thenUs` \ body' ->
    go (b:bs) body'
  where
    go []     body = returnUs body
    go (b:bs) body = go bs body 	`thenUs` \ body' ->
		     mkBind  b body'

-- body can't be Lam
mkBind (RecF prs) body = returnUs (Let (Rec prs) body)

mkBind (NonRecF bndr rhs dem floats) body
#ifdef DEBUG
  -- We shouldn't get let or case of the form v=w
  = if exprIsTrivial rhs 
	then pprTrace "mkBind" (ppr bndr <+> ppr rhs)
	     (mk_let bndr rhs dem floats body)
	else mk_let bndr rhs dem floats body

mk_let bndr rhs dem floats body
#endif
  | isUnLiftedType bndr_rep_ty
  = ASSERT( not (isUnboxedTupleType bndr_rep_ty) )
    mkBinds floats (Case rhs bndr [(DEFAULT, [], body)])

  | is_whnf
  = if is_strict then
	-- Strict let with WHNF rhs
	mkBinds floats $
	Let (NonRec bndr rhs) body
    else
	-- Lazy let with WHNF rhs; float until we find a strict binding
	let
	    (floats_out, floats_in) = splitFloats floats
	in
	mkBinds floats_in rhs	`thenUs` \ new_rhs ->
	mkBinds floats_out $
	Let (NonRec bndr new_rhs) body

  | otherwise 	-- Not WHNF
  = if is_strict then
	-- Strict let with non-WHNF rhs
	mkBinds floats (Case rhs bndr [(DEFAULT, [], body)])
    else
	-- Lazy let with non-WHNF rhs, so keep the floats in the RHS
	mkBinds floats rhs		`thenUs` \ new_rhs ->
	returnUs (Let (NonRec bndr new_rhs) body)
	
  where
    bndr_rep_ty = repType (idType bndr)
    is_strict   = isStrictDem dem
    is_whnf     = exprIsValue rhs

splitFloats fs@(NonRecF _ _ dem _ : _) 
  | isStrictDem dem = ([], fs)

splitFloats (f : fs) = case splitFloats fs of
		  	     (fs_out, fs_in) -> (f : fs_out, fs_in)

splitFloats [] = ([], [])

-- -----------------------------------------------------------------------------
-- Demands
-- -----------------------------------------------------------------------------

data RhsDemand
     = RhsDemand { isStrictDem :: Bool,  -- True => used at least once
                   isOnceDem   :: Bool   -- True => used at most once
                 }

mkDem :: Demand -> Bool -> RhsDemand
mkDem strict once = RhsDemand (isStrict strict) once

mkDemTy :: Demand -> Type -> RhsDemand
mkDemTy strict ty = RhsDemand (isStrict strict) (isOnceTy ty)

isOnceTy :: Type -> Bool
isOnceTy ty
  =
#ifdef USMANY
    opt_UsageSPOn &&  -- can't expect annotations if -fusagesp is off
#endif
    once
  where
    u = uaUTy ty
    once | u == usOnce  = True
         | u == usMany  = False
         | isTyVarTy u  = False  -- if unknown at compile-time, is Top ie usMany

bdrDem :: Id -> RhsDemand
bdrDem id = mkDem (idDemandInfo id) (isOnceTy (idType id))

safeDem, onceDem :: RhsDemand
safeDem = RhsDemand False False  -- always safe to use this
onceDem = RhsDemand False True   -- used at most once
\end{code}

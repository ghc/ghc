%
% (c) The University of Glasgow, 1994-2000
%
\section{Core pass to saturate constructors and PrimOps}

\begin{code}
module CoreSat (
      coreSatPgm, coreSatExpr
  ) where

#include "HsVersions.h"

import CoreUtils( exprIsTrivial, exprIsAtom, exprType, exprIsValue, etaExpand, exprArity )
import CoreFVs	( exprFreeVars )
import CoreLint	( endPass )
import CoreSyn
import Type	( Type, applyTy, splitFunTy_maybe, isTyVarTy,
		  isUnLiftedType, isUnboxedTupleType, repType,	
		  uaUTy, usOnce, usMany, seqType )
import Demand	( Demand, isStrict, wwLazy, StrictnessInfo(..) )
import Var 	( Id, TyVar, setTyVarUnique )
import VarSet
import IdInfo	( IdFlavour(..) )
import Id	( mkSysLocal, idType, idStrictness, idFlavour, idDemandInfo, idArity )

import UniqSupply
import Maybes
import OrdList
import ErrUtils
import CmdLineOpts
import Outputable
\end{code}

-- ---------------------------------------------------------------------------
-- Overview
-- ---------------------------------------------------------------------------

MAJOR CONSTRAINT: 
	By the time this pass happens, we have spat out tidied Core into
	the interface file, including all IdInfo.  

	So we must not change the arity of any top-level function,
	because we've already fixed it and put it out into the interface file.
	Nor must we change a value (e.g. constructor) into a thunk.

	It's ok to introduce extra bindings, which don't appear in the
	interface file.  We don't put arity info on these extra bindings,
	because they are never fully applied, so there's no chance of
	compiling just-a-fast-entry point for them.

Most of the contents of this pass used to be in CoreToStg.  The
primary goals here are:

1.  Saturate constructor and primop applications.

2.  Convert to A-normal form:

    * Use case for strict arguments:
	f E ==> case E of x -> f x
    	(where f is strict)

    * Use let for non-trivial lazy arguments
	f E ==> let x = E in f x
	(were f is lazy and x is non-trivial)

3.  Similarly, convert any unboxed lets into cases.
    [I'm experimenting with leaving 'ok-for-speculation' 
     rhss in let-form right up to this point.]

4.  Ensure that lambdas only occur as the RHS of a binding
    (The code generator can't deal with anything else.)

This is all done modulo type applications and abstractions, so that
when type erasure is done for conversion to STG, we don't end up with
any trivial or useless bindings.
  


-- -----------------------------------------------------------------------------
-- Top level stuff
-- -----------------------------------------------------------------------------

\begin{code}
coreSatPgm :: DynFlags -> [CoreBind] -> IO [CoreBind]
coreSatPgm dflags binds 
  = do	showPass dflags "CoreSat"
	us <- mkSplitUniqSupply 's'
	let new_binds = initUs_ us (coreSatTopBinds binds)
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

data FloatingBind = FloatLet CoreBind
		  | FloatCase Id CoreExpr

allLazy :: OrdList FloatingBind -> Bool
allLazy floats = foldOL check True floats
	       where
		 check (FloatLet _)    y = y
		 check (FloatCase _ _) y = False

coreSatTopBinds :: [CoreBind] -> UniqSM [CoreBind]
-- Very careful to preserve the arity of top-level functions
coreSatTopBinds [] = returnUs []

coreSatTopBinds (NonRec b r : binds)
  = coreSatTopRhs b r		`thenUs` \ (floats, r') ->
    coreSatTopBinds binds	`thenUs` \ binds' ->
    returnUs (floats ++ NonRec b r' : binds')

coreSatTopBinds (Rec prs : binds)
  = mapAndUnzipUs do_pair prs	`thenUs` \ (floats_s, prs') ->
    coreSatTopBinds binds	`thenUs` \ binds' ->
    returnUs (Rec (flattenBinds (concat floats_s) ++ prs') : binds')
  where
    do_pair (b,r) = coreSatTopRhs b r	`thenUs` \ (floats, r') ->
		    returnUs (floats, (b, r'))

coreSatTopRhs :: Id -> CoreExpr -> UniqSM ([CoreBind], CoreExpr)
-- The trick here is that if we see
--	x = $wC p $wJust q
-- we want to transform to
--	sat = \a -> $wJust a
--	x = $wC p sat q
-- and NOT to
--	x = let sat = \a -> $wJust a in $wC p sat q
--
-- The latter is bad because the thing was a value before, but
-- is a thunk now, and that's wrong because now x may need to
-- be in other bindings' SRTs.
-- This has to be right for recursive as well as non-recursive bindings
--
-- Notice that it's right to give sat vanilla IdInfo; in particular NoCafRefs
--
-- You might worry that arity might increase, thus
--	x = $wC a  ==>  x = \ b c -> $wC a b c
-- but the simpifier does eta expansion vigorously, so I don't think this 
-- can occur.  If it did, it would be a problem, because x's arity changes,
-- so we have an ASSERT to check.  (I use WARN so we can see the output.)

coreSatTopRhs b rhs
  = coreSatExprFloat rhs	`thenUs` \ (floats, rhs1) ->
    if exprIsValue rhs then
	ASSERT( allLazy floats )
        WARN( idArity b /= exprArity rhs1, ptext SLIT("Disaster!") <+> ppr b )
	returnUs ([bind | FloatLet bind <- fromOL floats], rhs1)
    else
	mkBinds floats rhs1	`thenUs` \ rhs2 ->
        WARN( idArity b /= exprArity rhs2, ptext SLIT("Disaster!") <+> ppr b )
	returnUs ([], rhs2)


coreSatBind :: CoreBind -> UniqSM (OrdList FloatingBind)
-- Used for non-top-level bindings
-- We return a *list* of bindings because we may start with
--	x* = f (g y)
-- where x is demanded, in which case we want to finish with
--	a = g y
--	x* = f a
-- And then x will actually end up case-bound

coreSatBind (NonRec binder rhs)
  = coreSatExprFloat rhs	`thenUs` \ (floats, new_rhs) ->
    mkNonRec binder (bdrDem binder) floats new_rhs
	-- NB: if there are any lambdas at the top of the RHS,
	-- the floats will be empty, so the arity won't be affected

coreSatBind (Rec pairs)
	-- Don't bother to try to float bindings out of RHSs
	-- (compare mkNonRec, which does try)
  = mapUs do_rhs pairs 			`thenUs` \ new_pairs ->
    returnUs (unitOL (FloatLet (Rec new_pairs)))
  where
    do_rhs (bndr,rhs) =	coreSatAnExpr rhs	`thenUs` \ new_rhs' ->
			returnUs (bndr,new_rhs')


-- ---------------------------------------------------------------------------
-- Making arguments atomic (function args & constructor args)
-- ---------------------------------------------------------------------------

-- This is where we arrange that a non-trivial argument is let-bound
coreSatArg :: CoreArg -> RhsDemand -> UniqSM (OrdList FloatingBind, CoreArg)
coreSatArg arg dem
  = coreSatExprFloat arg		`thenUs` \ (floats, arg') ->
    if needs_binding arg'
	then returnUs (floats, arg')
	else newVar (exprType arg')	`thenUs` \ v ->
	     mkNonRec v dem floats arg'	`thenUs` \ floats' -> 
	     returnUs (floats', Var v)

needs_binding | opt_KeepStgTypes = exprIsAtom
	      | otherwise	 = exprIsTrivial

-- ---------------------------------------------------------------------------
-- Dealing with expressions
-- ---------------------------------------------------------------------------

coreSatAnExpr :: CoreExpr -> UniqSM CoreExpr
coreSatAnExpr expr
  = coreSatExprFloat expr		`thenUs` \ (floats, expr) ->
    mkBinds floats expr


coreSatExprFloat :: CoreExpr -> UniqSM (OrdList FloatingBind, CoreExpr)
-- If
--	e  ===>  (bs, e')
-- then	
--	e = let bs in e'	(semantically, that is!)
--
-- For example
--	f (g x)	  ===>   ([v = g x], f v)

coreSatExprFloat (Var v)
  = maybeSaturate v (Var v) 0 (idType v) `thenUs` \ app ->
    returnUs (nilOL, app)

coreSatExprFloat (Lit lit)
  = returnUs (nilOL, Lit lit)

coreSatExprFloat (Let bind body)
  = coreSatBind bind			`thenUs` \ new_binds ->
    coreSatExprFloat body		`thenUs` \ (floats, new_body) ->
    returnUs (new_binds `appOL` floats, new_body)

coreSatExprFloat (Note n@(SCC _) expr)
  = coreSatAnExpr expr			`thenUs` \ expr ->
    deLam expr				`thenUs` \ expr ->
    returnUs (nilOL, Note n expr)

coreSatExprFloat (Note other_note expr)
  = coreSatExprFloat expr		`thenUs` \ (floats, expr) ->
    returnUs (floats, Note other_note expr)

coreSatExprFloat expr@(Type _)
  = returnUs (nilOL, expr)

coreSatExprFloat expr@(Lam _ _)
  = coreSatAnExpr body			`thenUs` \ body' ->
    returnUs (nilOL, mkLams bndrs body')
  where
    (bndrs,body) = collectBinders expr

coreSatExprFloat (Case scrut bndr alts)
  = coreSatExprFloat scrut		`thenUs` \ (floats, scrut) ->
    mapUs sat_alt alts			`thenUs` \ alts ->
    returnUs (floats, Case scrut bndr alts)
  where
    sat_alt (con, bs, rhs)
	  = coreSatAnExpr rhs	  	`thenUs` \ rhs ->
	    deLam rhs			`thenUs` \ rhs ->
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
    -- the head of the application, and the number of actual value arguments,
    -- all of which are used to possibly saturate this application if it
    -- has a constructor or primop at the head.

    collect_args
	:: CoreExpr
	-> Int				  -- current app depth
	-> UniqSM (CoreExpr,		  -- the rebuilt expression
		   (CoreExpr,Int),	  -- the head of the application,
				          -- and no. of args it was applied to
		   Type,		  -- type of the whole expr
		   OrdList FloatingBind,  -- any floats we pulled out
		   [Demand])		  -- remaining argument demands

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
	  returnUs (App fun' arg', hd, res_ty, fs `appOL` floats, ss_rest)

    collect_args (Var v) depth
	= returnUs (Var v, (Var v, depth), idType v, nilOL, stricts)
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
	= coreSatExprFloat fun	  		`thenUs` \ (fun_floats, fun) ->
	  newVar ty		 		`thenUs` \ fn_id ->
          mkNonRec fn_id onceDem fun_floats fun	`thenUs` \ floats ->
	  returnUs (Var fn_id, (Var fn_id, depth), ty, floats, [])
        where
	  ty = exprType fun

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
-- Precipitating the floating bindings
-- ---------------------------------------------------------------------------

-- mkNonRec is used for local bindings only, not top level
mkNonRec :: Id  -> RhsDemand 			-- Lhs: id with demand
	 -> OrdList FloatingBind -> CoreExpr	-- Rhs: let binds in body
	 -> UniqSM (OrdList FloatingBind)
mkNonRec bndr dem floats rhs
  | exprIsValue rhs && allLazy floats		-- Notably constructor applications
  = 	-- Why the test for allLazy? You might think that the only 
	-- floats we can get out of a value are eta expansions 
	-- e.g.  C $wJust ==> let s = \x -> $wJust x in C s
	-- Here we want to float the s binding.
	--
	-- But if the programmer writes this:
	--	f x = case x of { (a,b) -> \y -> a }
	-- then the strictness analyser may say that f has strictness "S"
	-- Later the eta expander will transform to
	--	f x y = case x of { (a,b) -> a }
	-- So now f has arity 2.  Now CoreSat may see
	--	v = f E
	-- so the E argument will turn into a FloatCase.  
	-- Indeed we should end up with
	--	v = case E of { r -> f r }
	-- That is, we should not float, even though (f r) is a value
    returnUs (floats `snocOL` FloatLet (NonRec bndr rhs))
    
  |  isUnLiftedType bndr_rep_ty	|| isStrictDem dem 
  = ASSERT( not (isUnboxedTupleType bndr_rep_ty) )
    returnUs (floats `snocOL` FloatCase bndr rhs)

  | otherwise
  = mkBinds floats rhs	`thenUs` \ rhs' ->
    returnUs (unitOL (FloatLet (NonRec bndr rhs')))

  where
    bndr_rep_ty  = repType (idType bndr)

mkBinds :: OrdList FloatingBind -> CoreExpr -> UniqSM CoreExpr
mkBinds binds body 
  | isNilOL binds = returnUs body
  | otherwise	  = deLam body		`thenUs` \ body' ->
		    returnUs (foldOL mk_bind body' binds)
  where
    mk_bind (FloatCase bndr rhs) body = Case rhs bndr [(DEFAULT, [], body)]
    mk_bind (FloatLet bind)      body = Let bind body

-- ---------------------------------------------------------------------------
-- Eliminate Lam as a non-rhs (STG doesn't have such a thing)
-- We arrange that they only show up as the RHS of a let(rec)
-- ---------------------------------------------------------------------------

deLam :: CoreExpr -> UniqSM CoreExpr	
-- Remove top level lambdas by let-bindinig

deLam (Note n expr)
  =	-- You can get things like
	-- 	case e of { p -> coerce t (\s -> ...) }
    deLam expr	`thenUs` \ expr' ->
    returnUs (Note n expr')

deLam expr 
  | null bndrs = returnUs expr
  | otherwise  = case tryEta bndrs body of
		   Just no_lam_result -> returnUs no_lam_result
		   Nothing	      -> newVar (exprType expr) `thenUs` \ fn ->
					 returnUs (Let (NonRec fn expr) (Var fn))
  where
    (bndrs,body) = collectBinders expr

-- Why try eta reduction?  Hasn't the simplifier already done eta?
-- But the simplifier only eta reduces if that leaves something
-- trivial (like f, or f Int).  But for deLam it would be enough to
-- get to a partial application, like (map f).

tryEta bndrs expr@(App _ _)
  | ok_to_eta_reduce f &&
    n_remaining >= 0 &&
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

	  -- we can't eta reduce something which must be saturated.
    ok_to_eta_reduce (Var f)
	 = case idFlavour f of
	      PrimOpId op  -> False
	      DataConId dc -> False
	      other 	   -> True
    ok_to_eta_reduce _ = False --safe. ToDo: generalise

tryEta bndrs (Let bind@(NonRec b r) body)
  | not (any (`elemVarSet` fvs) bndrs)
  = case tryEta bndrs body of
	Just e -> Just (Let bind e)
	Nothing -> Nothing
  where
    fvs = exprFreeVars r

tryEta bndrs _ = Nothing

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



%
% (c) The University of Glasgow, 1994-2006
%

Core pass to saturate constructors and PrimOps

\begin{code}
module CorePrep (
      corePrepPgm, corePrepExpr
  ) where

#include "HsVersions.h"

import CoreUtils hiding (exprIsTrivial)
import CoreFVs
import CoreLint
import CoreSyn
import Type
import Coercion
import TyCon
import NewDemand
import Var
import VarSet
import VarEnv
import Id
import DataCon
import PrimOp
import BasicTypes
import UniqSupply
import Maybes
import OrdList
import ErrUtils
import DynFlags
import Util
import Outputable
\end{code}

-- ---------------------------------------------------------------------------
-- Overview
-- ---------------------------------------------------------------------------

The goal of this pass is to prepare for code generation.

1.  Saturate constructor and primop applications.

2.  Convert to A-normal form; that is, function arguments
    are always variables.

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

5.  [Not any more; nuked Jun 2002] Do the seq/par munging.

6.  Clone all local Ids.
    This means that all such Ids are unique, rather than the 
    weaker guarantee of no clashes which the simplifier provides.
    And that is what the code generator needs.

    We don't clone TyVars. The code gen doesn't need that, 
    and doing so would be tiresome because then we'd need
    to substitute in types.


7.  Give each dynamic CCall occurrence a fresh unique; this is
    rather like the cloning step above.

8.  Inject bindings for the "implicit" Ids:
	* Constructor wrappers
	* Constructor workers
	* Record selectors
    We want curried definitions for all of these in case they
    aren't inlined by some caller.
	
This is all done modulo type applications and abstractions, so that
when type erasure is done for conversion to STG, we don't end up with
any trivial or useless bindings.

  

-- -----------------------------------------------------------------------------
-- Top level stuff
-- -----------------------------------------------------------------------------

\begin{code}
corePrepPgm :: DynFlags -> [CoreBind] -> [TyCon] -> IO [CoreBind]
corePrepPgm dflags binds data_tycons
  = do	showPass dflags "CorePrep"
	us <- mkSplitUniqSupply 's'

	let implicit_binds = mkDataConWorkers data_tycons
		-- NB: we must feed mkImplicitBinds through corePrep too
		-- so that they are suitably cloned and eta-expanded

	    binds_out = initUs_ us (
			  corePrepTopBinds binds	`thenUs` \ floats1 ->
			  corePrepTopBinds implicit_binds	`thenUs` \ floats2 ->
			  returnUs (deFloatTop (floats1 `appendFloats` floats2))
			)
	    
        endPass dflags "CorePrep" Opt_D_dump_prep binds_out
	return binds_out

corePrepExpr :: DynFlags -> CoreExpr -> IO CoreExpr
corePrepExpr dflags expr
  = do showPass dflags "CorePrep"
       us <- mkSplitUniqSupply 's'
       let new_expr = initUs_ us (corePrepAnExpr emptyCorePrepEnv expr)
       dumpIfSet_dyn dflags Opt_D_dump_prep "CorePrep" 
		     (ppr new_expr)
       return new_expr
\end{code}

-- -----------------------------------------------------------------------------
-- Implicit bindings
-- -----------------------------------------------------------------------------

Create any necessary "implicit" bindings for data con workers.  We
create the rather strange (non-recursive!) binding

	$wC = \x y -> $wC x y

i.e. a curried constructor that allocates.  This means that we can
treat the worker for a constructor like any other function in the rest
of the compiler.  The point here is that CoreToStg will generate a
StgConApp for the RHS, rather than a call to the worker (which would
give a loop).  As Lennart says: the ice is thin here, but it works.

Hmm.  Should we create bindings for dictionary constructors?  They are
always fully applied, and the bindings are just there to support
partial applications. But it's easier to let them through.

\begin{code}
mkDataConWorkers data_tycons
  = [ NonRec id (Var id)	-- The ice is thin here, but it works
    | tycon <- data_tycons, 	-- CorePrep will eta-expand it
      data_con <- tyConDataCons tycon,
      let id = dataConWorkId data_con ]
\end{code}
	

\begin{code}
-- ---------------------------------------------------------------------------
-- Dealing with bindings
-- ---------------------------------------------------------------------------

data FloatingBind = FloatLet CoreBind
		  | FloatCase Id CoreExpr Bool
			-- The bool indicates "ok-for-speculation"

data Floats = Floats OkToSpec (OrdList FloatingBind)

-- Can we float these binds out of the rhs of a let?  We cache this decision
-- to avoid having to recompute it in a non-linear way when there are
-- deeply nested lets.
data OkToSpec
   = NotOkToSpec 	-- definitely not
   | OkToSpec		-- yes
   | IfUnboxedOk	-- only if floating an unboxed binding is ok

emptyFloats :: Floats
emptyFloats = Floats OkToSpec nilOL

addFloat :: Floats -> FloatingBind -> Floats
addFloat (Floats ok_to_spec floats) new_float
  = Floats (combine ok_to_spec (check new_float)) (floats `snocOL` new_float)
  where
    check (FloatLet _)  	      = OkToSpec
    check (FloatCase _ _ ok_for_spec) 
	| ok_for_spec  =  IfUnboxedOk
	| otherwise    =  NotOkToSpec
	-- The ok-for-speculation flag says that it's safe to
	-- float this Case out of a let, and thereby do it more eagerly
	-- We need the top-level flag because it's never ok to float
	-- an unboxed binding to the top level

unitFloat :: FloatingBind -> Floats
unitFloat = addFloat emptyFloats

appendFloats :: Floats -> Floats -> Floats
appendFloats (Floats spec1 floats1) (Floats spec2 floats2)
  = Floats (combine spec1 spec2) (floats1 `appOL` floats2)

concatFloats :: [Floats] -> Floats
concatFloats = foldr appendFloats emptyFloats

combine NotOkToSpec _ = NotOkToSpec
combine _ NotOkToSpec = NotOkToSpec
combine IfUnboxedOk _ = IfUnboxedOk
combine _ IfUnboxedOk = IfUnboxedOk
combine _ _           = OkToSpec
    
instance Outputable FloatingBind where
  ppr (FloatLet bind)        = text "FloatLet" <+> ppr bind
  ppr (FloatCase b rhs spec) = text "FloatCase" <+> ppr b <+> ppr spec <+> equals <+> ppr rhs

deFloatTop :: Floats -> [CoreBind]
-- For top level only; we don't expect any FloatCases
deFloatTop (Floats _ floats)
  = foldrOL get [] floats
  where
    get (FloatLet b) bs = b:bs
    get b	     bs = pprPanic "corePrepPgm" (ppr b)

allLazy :: TopLevelFlag -> RecFlag -> Floats -> Bool
allLazy top_lvl is_rec (Floats ok_to_spec _)
  = case ok_to_spec of
	OkToSpec    -> True
	NotOkToSpec -> False
	IfUnboxedOk -> isNotTopLevel top_lvl && isNonRec is_rec

-- ---------------------------------------------------------------------------
-- 			Bindings
-- ---------------------------------------------------------------------------

corePrepTopBinds :: [CoreBind] -> UniqSM Floats
corePrepTopBinds binds 
  = go emptyCorePrepEnv binds
  where
    go env []		  = returnUs emptyFloats
    go env (bind : binds) = corePrepTopBind env bind	`thenUs` \ (env', bind') ->
			    go env' binds		`thenUs` \ binds' ->
			    returnUs (bind' `appendFloats` binds')

-- NB: we do need to float out of top-level bindings
-- Consider	x = length [True,False]
-- We want to get
--		s1 = False : []
--		s2 = True  : s1
--		x  = length s2

-- We return a *list* of bindings, because we may start with
--	x* = f (g y)
-- where x is demanded, in which case we want to finish with
--	a = g y
--	x* = f a
-- And then x will actually end up case-bound
--
-- What happens to the CafInfo on the floated bindings?  By
-- default, all the CafInfos will be set to MayHaveCafRefs,
-- which is safe.
--
-- This might be pessimistic, because eg. s1 & s2
-- might not refer to any CAFs and the GC will end up doing
-- more traversal than is necessary, but it's still better
-- than not floating the bindings at all, because then
-- the GC would have to traverse the structure in the heap
-- instead.  Given this, we decided not to try to get
-- the CafInfo on the floated bindings correct, because
-- it looks difficult.

--------------------------------
corePrepTopBind :: CorePrepEnv -> CoreBind -> UniqSM (CorePrepEnv, Floats)
corePrepTopBind env (NonRec bndr rhs) 
  = cloneBndr env bndr					`thenUs` \ (env', bndr') ->
    corePrepRhs TopLevel NonRecursive env (bndr, rhs)	`thenUs` \ (floats, rhs') -> 
    returnUs (env', addFloat floats (FloatLet (NonRec bndr' rhs')))

corePrepTopBind env (Rec pairs) = corePrepRecPairs TopLevel env pairs

--------------------------------
corePrepBind ::  CorePrepEnv -> CoreBind -> UniqSM (CorePrepEnv, Floats)
	-- This one is used for *local* bindings
corePrepBind env (NonRec bndr rhs)
  = etaExpandRhs bndr rhs				`thenUs` \ rhs1 ->
    corePrepExprFloat env rhs1				`thenUs` \ (floats, rhs2) ->
    cloneBndr env bndr					`thenUs` \ (_, bndr') ->
    mkLocalNonRec bndr' (bdrDem bndr) floats rhs2	`thenUs` \ (floats', bndr'') ->
	-- We want bndr'' in the envt, because it records
	-- the evaluated-ness of the binder
    returnUs (extendCorePrepEnv env bndr bndr'', floats')

corePrepBind env (Rec pairs) = corePrepRecPairs NotTopLevel env pairs

--------------------------------
corePrepRecPairs :: TopLevelFlag -> CorePrepEnv
		 -> [(Id,CoreExpr)]	-- Recursive bindings
		 -> UniqSM (CorePrepEnv, Floats)
-- Used for all recursive bindings, top level and otherwise
corePrepRecPairs lvl env pairs
  = cloneBndrs env (map fst pairs)				`thenUs` \ (env', bndrs') ->
    mapAndUnzipUs (corePrepRhs lvl Recursive env') pairs	`thenUs` \ (floats_s, rhss') ->
    returnUs (env', unitFloat (FloatLet (Rec (flatten (concatFloats floats_s) bndrs' rhss'))))
  where
	-- Flatten all the floats, and the currrent
	-- group into a single giant Rec
    flatten (Floats _ floats) bndrs rhss = foldrOL get (bndrs `zip` rhss) floats

    get (FloatLet (NonRec b r)) prs2 = (b,r) : prs2
    get (FloatLet (Rec prs1))   prs2 = prs1 ++ prs2
    get b			prs2 = pprPanic "corePrepRecPairs" (ppr b)

--------------------------------
corePrepRhs :: TopLevelFlag -> RecFlag
	    -> CorePrepEnv -> (Id, CoreExpr)
	    -> UniqSM (Floats, CoreExpr)
-- Used for top-level bindings, and local recursive bindings
corePrepRhs top_lvl is_rec env (bndr, rhs)
  = etaExpandRhs bndr rhs	`thenUs` \ rhs' ->
    corePrepExprFloat env rhs'	`thenUs` \ floats_w_rhs ->
    floatRhs top_lvl is_rec bndr floats_w_rhs


-- ---------------------------------------------------------------------------
-- Making arguments atomic (function args & constructor args)
-- ---------------------------------------------------------------------------

-- This is where we arrange that a non-trivial argument is let-bound
corePrepArg :: CorePrepEnv -> CoreArg -> RhsDemand
	   -> UniqSM (Floats, CoreArg)
corePrepArg env arg dem
  = corePrepExprFloat env arg		`thenUs` \ (floats, arg') ->
    if exprIsTrivial arg'
    then returnUs (floats, arg')
    else newVar (exprType arg')			`thenUs` \ v ->
	 mkLocalNonRec v dem floats arg'	`thenUs` \ (floats', v') -> 
	 returnUs (floats', Var v')

-- version that doesn't consider an scc annotation to be trivial.
exprIsTrivial (Var v)		       = True
exprIsTrivial (Type _)	      	       = True
exprIsTrivial (Lit lit)       	       = True
exprIsTrivial (App e arg)     	       = isTypeArg arg && exprIsTrivial e
exprIsTrivial (Note (SCC _) e) 	       = False
exprIsTrivial (Note _ e)      	       = exprIsTrivial e
exprIsTrivial (Cast e co)              = exprIsTrivial e
exprIsTrivial (Lam b body) | isTyVar b = exprIsTrivial body
exprIsTrivial other	      	       = False

-- ---------------------------------------------------------------------------
-- Dealing with expressions
-- ---------------------------------------------------------------------------

corePrepAnExpr :: CorePrepEnv -> CoreExpr -> UniqSM CoreExpr
corePrepAnExpr env expr
  = corePrepExprFloat env expr		`thenUs` \ (floats, expr) ->
    mkBinds floats expr


corePrepExprFloat :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CoreExpr)
-- If
--	e  ===>  (bs, e')
-- then	
--	e = let bs in e'	(semantically, that is!)
--
-- For example
--	f (g x)	  ===>   ([v = g x], f v)

corePrepExprFloat env (Var v)
  = fiddleCCall v				`thenUs` \ v1 ->
    let 
	v2 = lookupCorePrepEnv env v1
    in
    maybeSaturate v2 (Var v2) 0 emptyFloats (idType v2)

corePrepExprFloat env expr@(Type _)
  = returnUs (emptyFloats, expr)

corePrepExprFloat env expr@(Lit lit)
  = returnUs (emptyFloats, expr)

corePrepExprFloat env (Let bind body)
  = corePrepBind env bind		`thenUs` \ (env', new_binds) ->
    corePrepExprFloat env' body		`thenUs` \ (floats, new_body) ->
    returnUs (new_binds `appendFloats` floats, new_body)

corePrepExprFloat env (Note n@(SCC _) expr)
  = corePrepAnExpr env expr		`thenUs` \ expr1 ->
    deLamFloat expr1			`thenUs` \ (floats, expr2) ->
    returnUs (floats, Note n expr2)

corePrepExprFloat env (Note other_note expr)
  = corePrepExprFloat env expr		`thenUs` \ (floats, expr') ->
    returnUs (floats, Note other_note expr')

corePrepExprFloat env (Cast expr co)
  = corePrepExprFloat env expr		`thenUs` \ (floats, expr') ->
    returnUs (floats, Cast expr' co)

corePrepExprFloat env expr@(Lam _ _)
  = cloneBndrs env bndrs		`thenUs` \ (env', bndrs') ->
    corePrepAnExpr env' body		`thenUs` \ body' ->
    returnUs (emptyFloats, mkLams bndrs' body')
  where
    (bndrs,body) = collectBinders expr

corePrepExprFloat env (Case scrut bndr ty alts)
  = corePrepExprFloat env scrut		`thenUs` \ (floats1, scrut1) ->
    deLamFloat scrut1			`thenUs` \ (floats2, scrut2) ->
    let
	bndr1 = bndr `setIdUnfolding` evaldUnfolding
	-- Record that the case binder is evaluated in the alternatives
    in
    cloneBndr env bndr1			`thenUs` \ (env', bndr2) ->
    mapUs (sat_alt env') alts		`thenUs` \ alts' ->
    returnUs (floats1 `appendFloats` floats2 , Case scrut2 bndr2 ty alts')
  where
    sat_alt env (con, bs, rhs)
	  = cloneBndrs env bs		`thenUs` \ (env2, bs') ->
	    corePrepAnExpr env2 rhs  	`thenUs` \ rhs1 ->
	    deLam rhs1			`thenUs` \ rhs2 ->
	    returnUs (con, bs', rhs2)

corePrepExprFloat env expr@(App _ _)
  = collect_args expr 0  `thenUs` \ (app, (head,depth), ty, floats, ss) ->
    ASSERT(null ss)	-- make sure we used all the strictness info

	-- Now deal with the function
    case head of
      Var fn_id -> maybeSaturate fn_id app depth floats ty
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
		   Floats, 		  -- any floats we pulled out
		   [Demand])		  -- remaining argument demands

    collect_args (App fun arg@(Type arg_ty)) depth
        = collect_args fun depth   `thenUs` \ (fun',hd,fun_ty,floats,ss) ->
	  returnUs (App fun' arg, hd, applyTy fun_ty arg_ty, floats, ss)

    collect_args (App fun arg) depth
        = collect_args fun (depth+1)   `thenUs` \ (fun',hd,fun_ty,floats,ss) ->
	  let
	      (ss1, ss_rest)   = case ss of
				   (ss1:ss_rest) -> (ss1,     ss_rest)
				   []	         -> (lazyDmd, [])
              (arg_ty, res_ty) = expectJust "corePrepExprFloat:collect_args" $
                                 splitFunTy_maybe fun_ty
	  in
	  corePrepArg env arg (mkDemTy ss1 arg_ty)	`thenUs` \ (fs, arg') ->
	  returnUs (App fun' arg', hd, res_ty, fs `appendFloats` floats, ss_rest)

    collect_args (Var v) depth
	= fiddleCCall v `thenUs` \ v1 ->
	  let 
		v2 = lookupCorePrepEnv env v1
	  in
	  returnUs (Var v2, (Var v2, depth), idType v2, emptyFloats, stricts)
	where
	  stricts = case idNewStrictness v of
			StrictSig (DmdType _ demands _)
			    | listLengthCmp demands depth /= GT -> demands
			            -- length demands <= depth
			    | otherwise                         -> []
		-- If depth < length demands, then we have too few args to 
		-- satisfy strictness  info so we have to  ignore all the 
		-- strictness info, e.g. + (error "urk")
		-- Here, we can't evaluate the arg strictly, because this 
		-- partial application might be seq'd

    collect_args (Cast fun co) depth
        = let (_ty1,ty2) = coercionKind co in
          collect_args fun depth  `thenUs` \ (fun', hd, fun_ty, floats, ss) ->
	  returnUs (Cast fun' co, hd, ty2, floats, ss)
          
    collect_args (Note note fun) depth
	| ignore_note note 	-- Drop these notes altogether
				-- They aren't used by the code generator
        = collect_args fun depth   `thenUs` \ (fun', hd, fun_ty, floats, ss) ->
	  returnUs (fun', hd, fun_ty, floats, ss)

	-- N-variable fun, better let-bind it
	-- ToDo: perhaps we can case-bind rather than let-bind this closure,
	-- since it is sure to be evaluated.
    collect_args fun depth
	= corePrepExprFloat env fun			`thenUs` \ (fun_floats, fun') ->
	  newVar ty			 		`thenUs` \ fn_id ->
          mkLocalNonRec fn_id onceDem fun_floats fun'	`thenUs` \ (floats, fn_id') ->
	  returnUs (Var fn_id', (Var fn_id', depth), ty, floats, [])
        where
	  ty = exprType fun

    ignore_note	(CoreNote _) = True 
    ignore_note	InlineMe     = True
    ignore_note	_other       = False
	-- We don't ignore SCCs, since they require some code generation

------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

-- maybeSaturate deals with saturating primops and constructors
-- The type is the type of the entire application
maybeSaturate :: Id -> CoreExpr -> Int -> Floats -> Type -> UniqSM (Floats, CoreExpr)
maybeSaturate fn expr n_args floats ty
  | Just DataToTagOp <- isPrimOpId_maybe fn	-- DataToTag must have an evaluated arg
						-- A gruesome special case
  = saturate_it		`thenUs` \ sat_expr ->

	-- OK, now ensure that the arg is evaluated.
	-- But (sigh) take into account the lambdas we've now introduced
    let 
	(eta_bndrs, eta_body) = collectBinders sat_expr
    in
    eval_data2tag_arg eta_body	`thenUs` \ (eta_floats, eta_body') -> 
    if null eta_bndrs then
	returnUs (floats `appendFloats` eta_floats, eta_body')
    else
	mkBinds eta_floats eta_body'		`thenUs` \ eta_body'' ->
	returnUs (floats, mkLams eta_bndrs eta_body'')

  | hasNoBinding fn = saturate_it	`thenUs` \ sat_expr ->
		      returnUs (floats, sat_expr)

  | otherwise       = returnUs (floats, expr)

  where
    fn_arity	 = idArity fn
    excess_arity = fn_arity - n_args

    saturate_it :: UniqSM CoreExpr
    saturate_it | excess_arity == 0 = returnUs expr
		| otherwise	    = getUniquesUs		`thenUs` \ us ->
				      returnUs (etaExpand excess_arity us expr ty)

	-- Ensure that the argument of DataToTagOp is evaluated
    eval_data2tag_arg :: CoreExpr -> UniqSM (Floats, CoreExpr)
    eval_data2tag_arg app@(fun `App` arg)
	| exprIsHNF arg		-- Includes nullary constructors
	= returnUs (emptyFloats, app)	-- The arg is evaluated
	| otherwise			-- Arg not evaluated, so evaluate it
	= newVar (exprType arg)		`thenUs` \ arg_id ->
	  let 
	     arg_id1 = setIdUnfolding arg_id evaldUnfolding
	  in
	  returnUs (unitFloat (FloatCase arg_id1 arg False ),
		    fun `App` Var arg_id1)

    eval_data2tag_arg (Note note app)	-- Scc notes can appear
	= eval_data2tag_arg app		`thenUs` \ (floats, app') ->
	  returnUs (floats, Note note app')

    eval_data2tag_arg other	-- Should not happen
	= pprPanic "eval_data2tag" (ppr other)


-- ---------------------------------------------------------------------------
-- Precipitating the floating bindings
-- ---------------------------------------------------------------------------

floatRhs :: TopLevelFlag -> RecFlag
	 -> Id
	 -> (Floats, CoreExpr)	-- Rhs: let binds in body
	 -> UniqSM (Floats, 	-- Floats out of this bind
		    CoreExpr)	-- Final Rhs

floatRhs top_lvl is_rec bndr (floats, rhs)
  | isTopLevel top_lvl || exprIsHNF rhs,	-- Float to expose value or 
    allLazy top_lvl is_rec floats 		-- at top level
  = 	-- Why the test for allLazy? 
	--	v = f (x `divInt#` y)
	-- we don't want to float the case, even if f has arity 2,
	-- because floating the case would make it evaluated too early
    returnUs (floats, rhs)
    
  | otherwise
	-- Don't float; the RHS isn't a value
  = mkBinds floats rhs		`thenUs` \ rhs' ->
    returnUs (emptyFloats, rhs')

-- mkLocalNonRec is used only for *nested*, *non-recursive* bindings
mkLocalNonRec :: Id  -> RhsDemand 	-- Lhs: id with demand
	      -> Floats -> CoreExpr	-- Rhs: let binds in body
	      -> UniqSM (Floats, Id)	-- The new Id may have an evaldUnfolding, 
					-- to record that it's been evaluated

mkLocalNonRec bndr dem floats rhs
  | isUnLiftedType (idType bndr)
	-- If this is an unlifted binding, we always make a case for it.
  = ASSERT( not (isUnboxedTupleType (idType bndr)) )
    let
	float = FloatCase bndr rhs (exprOkForSpeculation rhs)
    in
    returnUs (addFloat floats float, evald_bndr)

  | isStrict dem 
	-- It's a strict let so we definitely float all the bindings
 = let		-- Don't make a case for a value binding,
		-- even if it's strict.  Otherwise we get
		-- 	case (\x -> e) of ...!
	float | exprIsHNF rhs = FloatLet (NonRec bndr rhs)
	      | otherwise	= FloatCase bndr rhs (exprOkForSpeculation rhs)
    in
    returnUs (addFloat floats float, evald_bndr)

  | otherwise
  = floatRhs NotTopLevel NonRecursive bndr (floats, rhs)	`thenUs` \ (floats', rhs') ->
    returnUs (addFloat floats' (FloatLet (NonRec bndr rhs')),
	      if exprIsHNF rhs' then evald_bndr else bndr)

  where
    evald_bndr = bndr `setIdUnfolding` evaldUnfolding
	-- Record if the binder is evaluated


mkBinds :: Floats -> CoreExpr -> UniqSM CoreExpr
mkBinds (Floats _ binds) body 
  | isNilOL binds = returnUs body
  | otherwise	  = deLam body		`thenUs` \ body' ->
			-- Lambdas are not allowed as the body of a 'let'
		    returnUs (foldrOL mk_bind body' binds)
  where
    mk_bind (FloatCase bndr rhs _) body = Case rhs bndr (exprType body) [(DEFAULT, [], body)]
    mk_bind (FloatLet bind)        body = Let bind body

etaExpandRhs bndr rhs
  = 	-- Eta expand to match the arity claimed by the binder
	-- Remember, after CorePrep we must not change arity
	--
	-- Eta expansion might not have happened already, 
	-- because it is done by the simplifier only when 
	-- there at least one lambda already.
	-- 
	-- NB1:we could refrain when the RHS is trivial (which can happen
	--     for exported things).  This would reduce the amount of code
	--     generated (a little) and make things a little words for
	--     code compiled without -O.  The case in point is data constructor
	--     wrappers.
	--
	-- NB2: we have to be careful that the result of etaExpand doesn't
	--    invalidate any of the assumptions that CorePrep is attempting
	--    to establish.  One possible cause is eta expanding inside of
	--    an SCC note - we're now careful in etaExpand to make sure the
	--    SCC is pushed inside any new lambdas that are generated.
	--
	-- NB3: It's important to do eta expansion, and *then* ANF-ising
	--		f = /\a -> g (h 3)	-- h has arity 2
	-- If we ANF first we get
	--		f = /\a -> let s = h 3 in g s
	-- and now eta expansion gives
	-- 		f = /\a -> \ y -> (let s = h 3 in g s) y
	-- which is horrible.
	-- Eta expanding first gives
	--		f = /\a -> \y -> let s = h 3 in g s y
	--
    getUniquesUs		`thenUs` \ us ->
    returnUs (etaExpand arity us rhs (idType bndr))
  where
	-- For a GlobalId, take the Arity from the Id.
	-- It was set in CoreTidy and must not change
	-- For all others, just expand at will
    arity | isGlobalId bndr = idArity bndr
	  | otherwise	    = exprArity rhs

-- ---------------------------------------------------------------------------
-- Eliminate Lam as a non-rhs (STG doesn't have such a thing)
-- We arrange that they only show up as the RHS of a let(rec)
-- ---------------------------------------------------------------------------

deLam :: CoreExpr -> UniqSM CoreExpr
-- Takes an expression that may be a lambda, 
-- and returns one that definitely isn't:
--	(\x.e) ==>  let f = \x.e in f
deLam expr = 
  deLamFloat expr   `thenUs` \ (floats, expr) ->
  mkBinds floats expr


deLamFloat :: CoreExpr -> UniqSM (Floats, CoreExpr)
-- Remove top level lambdas by let-bindinig

deLamFloat (Note n expr)
  =	-- You can get things like
	-- 	case e of { p -> coerce t (\s -> ...) }
    deLamFloat expr	`thenUs` \ (floats, expr') ->
    returnUs (floats, Note n expr')

deLamFloat (Cast e co)
  = deLamFloat e	`thenUs` \ (floats, e') ->
    returnUs (floats, Cast e' co)

deLamFloat expr 
  | null bndrs = returnUs (emptyFloats, expr)
  | otherwise 
  = case tryEta bndrs body of
      Just no_lam_result -> returnUs (emptyFloats, no_lam_result)
      Nothing	         -> newVar (exprType expr)	`thenUs` \ fn ->
			    returnUs (unitFloat (FloatLet (NonRec fn expr)), 
				      Var fn)
  where
    (bndrs,body) = collectBinders expr

-- Why try eta reduction?  Hasn't the simplifier already done eta?
-- But the simplifier only eta reduces if that leaves something
-- trivial (like f, or f Int).  But for deLam it would be enough to
-- get to a partial application:
-- 	\xs. map f xs ==> map f

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
    ok bndr other     = False

	  -- we can't eta reduce something which must be saturated.
    ok_to_eta_reduce (Var f) = not (hasNoBinding f)
    ok_to_eta_reduce _       = False --safe. ToDo: generalise

tryEta bndrs (Let bind@(NonRec b r) body)
  | not (any (`elemVarSet` fvs) bndrs)
  = case tryEta bndrs body of
	Just e -> Just (Let bind e)
	Nothing -> Nothing
  where
    fvs = exprFreeVars r

tryEta bndrs _ = Nothing
\end{code}


-- -----------------------------------------------------------------------------
-- Demands
-- -----------------------------------------------------------------------------

\begin{code}
data RhsDemand
     = RhsDemand { isStrict :: Bool,  -- True => used at least once
                   isOnceDem   :: Bool   -- True => used at most once
                 }

mkDem :: Demand -> Bool -> RhsDemand
mkDem strict once = RhsDemand (isStrictDmd strict) once

mkDemTy :: Demand -> Type -> RhsDemand
mkDemTy strict ty = RhsDemand (isStrictDmd strict) 
			      False {- For now -}

bdrDem :: Id -> RhsDemand
bdrDem id = mkDem (idNewDemandInfo id)
		  False {- For now -}

-- safeDem :: RhsDemand
-- safeDem = RhsDemand False False  -- always safe to use this

onceDem :: RhsDemand
onceDem = RhsDemand False True   -- used at most once
\end{code}




%************************************************************************
%*									*
\subsection{Cloning}
%*									*
%************************************************************************

\begin{code}
-- ---------------------------------------------------------------------------
-- 			The environment
-- ---------------------------------------------------------------------------

data CorePrepEnv = CPE (IdEnv Id)	-- Clone local Ids

emptyCorePrepEnv :: CorePrepEnv
emptyCorePrepEnv = CPE emptyVarEnv

extendCorePrepEnv :: CorePrepEnv -> Id -> Id -> CorePrepEnv
extendCorePrepEnv (CPE env) id id' = CPE (extendVarEnv env id id')

lookupCorePrepEnv :: CorePrepEnv -> Id -> Id
lookupCorePrepEnv (CPE env) id
  = case lookupVarEnv env id of
	Nothing	 -> id
	Just id' -> id'

------------------------------------------------------------------------------
-- Cloning binders
-- ---------------------------------------------------------------------------

cloneBndrs :: CorePrepEnv -> [Var] -> UniqSM (CorePrepEnv, [Var])
cloneBndrs env bs = mapAccumLUs cloneBndr env bs

cloneBndr  :: CorePrepEnv -> Var -> UniqSM (CorePrepEnv, Var)
cloneBndr env bndr
  | isLocalId bndr
  = getUniqueUs   `thenUs` \ uniq ->
    let
	bndr' = setVarUnique bndr uniq
    in
    returnUs (extendCorePrepEnv env bndr bndr', bndr')

  | otherwise	-- Top level things, which we don't want
		-- to clone, have become GlobalIds by now
		-- And we don't clone tyvars
  = returnUs (env, bndr)
  

------------------------------------------------------------------------------
-- Cloning ccall Ids; each must have a unique name,
-- to give the code generator a handle to hang it on
-- ---------------------------------------------------------------------------

fiddleCCall :: Id -> UniqSM Id
fiddleCCall id 
  | isFCallId id = getUniqueUs   	`thenUs` \ uniq ->
		   returnUs (id `setVarUnique` uniq)
  | otherwise    = returnUs id

------------------------------------------------------------------------------
-- Generating new binders
-- ---------------------------------------------------------------------------

newVar :: Type -> UniqSM Id
newVar ty
 = seqType ty			`seq`
   getUniqueUs	 		`thenUs` \ uniq ->
   returnUs (mkSysLocal FSLIT("sat") uniq ty)
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section{SetLevels}

We attach binding levels to Core bindings, in preparation for floating
outwards (@FloatOut@).

We also let-ify many applications (notably case scrutinees), so they
will have a fighting chance of being floated sensible.

\begin{code}
#include "HsVersions.h"

module SetLevels (
	setLevels,

	Level(..), tOP_LEVEL,

	incMinorLvl, ltMajLvl, ltLvl, isTopLvl
-- not exported: , incMajorLvl, isTopMajLvl, unTopify
    ) where

IMP_Ubiq(){-uitous-}

import AnnCoreSyn
import CoreSyn

import CoreUtils	( coreExprType )
import CoreUnfold	( whnfOrBottom )
import FreeVars		-- all of it
import Id		( idType, mkSysLocal, 
			  nullIdEnv, addOneToIdEnv, growIdEnvList,
			  unionManyIdSets, minusIdSet, mkIdSet,
			  idSetToList, SYN_IE(Id),
			  lookupIdEnv, SYN_IE(IdEnv)
			)
import Pretty		( ptext, hcat, char, int )
import SrcLoc		( noSrcLoc )
import Type		( isPrimType, mkTyVarTys, mkForAllTys, SYN_IE(Type) )
import TyVar		( nullTyVarEnv, addOneToTyVarEnv,
			  growTyVarEnvList, lookupTyVarEnv,
			  tyVarSetToList,
			  SYN_IE(TyVarEnv), SYN_IE(TyVar),
			  unionManyTyVarSets
			)
import UniqSupply	( thenUs, returnUs, mapUs, mapAndUnzipUs,
			  mapAndUnzip3Us, getUnique, SYN_IE(UniqSM),
			  UniqSupply
			)
import Usage		( SYN_IE(UVar) )
import Util		( mapAccumL, zipWithEqual, zipEqual, panic, assertPanic )
#if __GLASGOW_HASKELL__ >= 202
import Outputable       ( Outputable(..) )
#endif

isLeakFreeType x y = False -- safe option; ToDo
\end{code}

%************************************************************************
%*									*
\subsection{Level numbers}
%*									*
%************************************************************************

\begin{code}
data Level
  = Top		-- Means *really* the top level.
  | Level   Int	-- Level number of enclosing lambdas
	    Int	-- Number of big-lambda and/or case expressions between
		-- here and the nearest enclosing lambda
\end{code}

The {\em level number} on a (type-)lambda-bound variable is the
nesting depth of the (type-)lambda which binds it.  On an expression,
it's the maximum level number of its free (type-)variables.  On a
let(rec)-bound variable, it's the level of its RHS.  On a case-bound
variable, it's the number of enclosing lambdas.

Top-level variables: level~0.  Those bound on the RHS of a top-level
definition but ``before'' a lambda; e.g., the \tr{x} in (levels shown
as ``subscripts'')...
\begin{verbatim}
a_0 = let  b_? = ...  in
	   x_1 = ... b ... in ...
\end{verbatim}

Level 0 0 will make something get floated to a top-level "equals",
@Top@ makes it go right to the top.

The main function @lvlExpr@ carries a ``context level'' (@ctxt_lvl@).
That's meant to be the level number of the enclosing binder in the
final (floated) program.  If the level number of a sub-expression is
less than that of the context, then it might be worth let-binding the
sub-expression so that it will indeed float. This context level starts
at @Level 0 0@; it is never @Top@.

\begin{code}
type LevelledExpr  = GenCoreExpr    (Id, Level) Id TyVar UVar
type LevelledArg   = GenCoreArg			Id TyVar UVar
type LevelledBind  = GenCoreBinding (Id, Level) Id TyVar UVar

type LevelEnvs = (IdEnv    Level, -- bind Ids to levels
		  TyVarEnv Level) -- bind type variables to levels

tOP_LEVEL = Top

incMajorLvl :: Level -> Level
incMajorLvl Top			= Level 1 0
incMajorLvl (Level major minor) = Level (major+1) 0

incMinorLvl :: Level -> Level
incMinorLvl Top			= Level 0 1
incMinorLvl (Level major minor) = Level major (minor+1)

maxLvl :: Level -> Level -> Level
maxLvl Top l2 = l2
maxLvl l1 Top = l1
maxLvl l1@(Level maj1 min1) l2@(Level maj2 min2)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise					   = l2

ltLvl :: Level -> Level -> Bool
ltLvl l1  		Top               = False
ltLvl Top 		(Level _ _)       = True
ltLvl (Level maj1 min1) (Level maj2 min2)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl l1  		Top            = False
ltMajLvl Top		(Level 0 _)    = False
ltMajLvl Top 		(Level _ _)    = True
ltMajLvl (Level maj1 _) (Level maj2 _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl Top   = True
isTopLvl other = False

isTopMajLvl :: Level -> Bool -- Tells if it's the top *lambda* level
isTopMajLvl Top		  = True
isTopMajLvl (Level maj _) = maj == 0

unTopify :: Level -> Level
unTopify Top = Level 0 0
unTopify lvl = lvl

instance Outputable Level where
  ppr sty Top		  = ptext SLIT("<Top>")
  ppr sty (Level maj min) = hcat [ char '<', int maj, char ',', int min, char '>' ]
\end{code}

%************************************************************************
%*									*
\subsection{Main level-setting code}
%*									*
%************************************************************************

\begin{code}
setLevels :: [CoreBinding]
	  -> UniqSupply
	  -> [LevelledBind]

setLevels binds us
  = do_them binds us
  where
    -- "do_them"'s main business is to thread the monad along
    -- It gives each top binding the same empty envt, because
    -- things unbound in the envt have level number zero implicitly
    do_them :: [CoreBinding] -> LvlM [LevelledBind]

    do_them [] = returnLvl []
    do_them (b:bs)
      = lvlTopBind b	`thenLvl` \ (lvld_bind, _) ->
	do_them bs	 `thenLvl` \ lvld_binds ->
    	returnLvl (lvld_bind ++ lvld_binds)

initial_envs = (nullIdEnv, nullTyVarEnv)

lvlTopBind (NonRec binder rhs)
  = lvlBind (Level 0 0) initial_envs (AnnNonRec binder (freeVars rhs))
					-- Rhs can have no free vars!

lvlTopBind (Rec pairs)
  = lvlBind (Level 0 0) initial_envs (AnnRec [(b,freeVars rhs) | (b,rhs) <- pairs])
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

The binding stuff works for top level too.

\begin{code}
type CoreBindingWithFVs = AnnCoreBinding Id Id TyVar UVar FVInfo

lvlBind :: Level
	-> LevelEnvs
	-> CoreBindingWithFVs
	-> LvlM ([LevelledBind], LevelEnvs)

lvlBind ctxt_lvl envs@(venv, tenv) (AnnNonRec name rhs)
  = setFloatLevel True {- Already let-bound -}
	ctxt_lvl envs rhs ty 	`thenLvl` \ (final_lvl, rhs') ->
    let
	new_envs = (addOneToIdEnv venv name final_lvl, tenv)
    in
    returnLvl ([NonRec (name, final_lvl) rhs'], new_envs)
  where
    ty = idType name


lvlBind ctxt_lvl envs@(venv, tenv) (AnnRec pairs)
  = decideRecFloatLevel ctxt_lvl envs binders rhss
				`thenLvl` \ (final_lvl, extra_binds, rhss') ->
    let
	binders_w_lvls = binders `zip` repeat final_lvl
	new_envs       = (growIdEnvList venv binders_w_lvls, tenv)
    in
    returnLvl (extra_binds ++ [Rec (zipEqual "lvlBind" binders_w_lvls rhss')], new_envs)
  where
    (binders,rhss) = unzip pairs
\end{code}

%************************************************************************
%*									*
\subsection{Setting expression levels}
%*									*
%************************************************************************

\begin{code}
lvlExpr :: Level		-- ctxt_lvl: Level of enclosing expression
	-> LevelEnvs		-- Level of in-scope names/tyvars
	-> CoreExprWithFVs	-- input expression
	-> LvlM LevelledExpr	-- Result expression
\end{code}

The @ctxt_lvl@ is, roughly, the level of the innermost enclosing
binder.

Here's an example

	v = \x -> ...\y -> let r = case (..x..) of
					..x..
			   in ..

When looking at the rhs of @r@, @ctxt_lvl@ will be 1 because that's
the level of @r@, even though it's inside a level-2 @\y@.  It's
important that @ctxt_lvl@ is 1 and not 2 in @r@'s rhs, because we
don't want @lvlExpr@ to turn the scrutinee of the @case@ into an MFE
--- because it isn't a *maximal* free expression.

If there were another lambda in @r@'s rhs, it would get level-2 as well.

\begin{code}
lvlExpr _ _ (_, AnnVar v)     	 = returnLvl (Var v)
lvlExpr _ _ (_, AnnLit l) 	 = returnLvl (Lit l)
lvlExpr _ _ (_, AnnCon con args) = returnLvl (Con con args)
lvlExpr _ _ (_, AnnPrim op args) = returnLvl (Prim op args)

lvlExpr ctxt_lvl envs@(venv, tenv) (_, AnnApp fun arg)
  = lvlExpr ctxt_lvl envs fun		`thenLvl` \ fun' ->
    returnLvl (App fun' arg)

lvlExpr ctxt_lvl envs (_, AnnSCC cc expr)
  = lvlExpr ctxt_lvl envs expr 		`thenLvl` \ expr' ->
    returnLvl (SCC cc expr')

lvlExpr ctxt_lvl envs (_, AnnCoerce c ty expr)
  = lvlExpr ctxt_lvl envs expr 		`thenLvl` \ expr' ->
    returnLvl (Coerce c ty expr')

-- We don't split adjacent lambdas.  That is, given
--	\x y -> (x+1,y)
-- we don't float to give 
--	\x -> let v = x+y in \y -> (v,y)
-- Why not?  Because partial applications are fairly rare, and splitting
-- lambdas makes them more expensive.

lvlExpr ctxt_lvl envs@(venv, tenv) (_, AnnLam (ValBinder arg) rhs)
  = lvlMFE incd_lvl (new_venv, tenv) body `thenLvl` \ body' ->
    returnLvl (foldr (Lam . ValBinder) body' lvld_args)
  where
    incd_lvl     = incMajorLvl ctxt_lvl
    (args, body) = annCollectValBinders rhs
    lvld_args    = [(a,incd_lvl) | a <- (arg:args)]
    new_venv     = growIdEnvList venv lvld_args

-- We don't need to play such tricks for type lambdas, because
-- they don't get annotated

lvlExpr ctxt_lvl (venv, tenv) (_, AnnLam (TyBinder tyvar) body)
  = lvlExpr incd_lvl (venv, new_tenv) body	`thenLvl` \ body' ->
    returnLvl (Lam (TyBinder tyvar) body')
  where
    incd_lvl = incMinorLvl ctxt_lvl
    new_tenv = addOneToTyVarEnv tenv tyvar incd_lvl

lvlExpr ctxt_lvl (venv, tenv) (_, AnnLam (UsageBinder u) e)
  = panic "SetLevels.lvlExpr:AnnLam UsageBinder"

lvlExpr ctxt_lvl envs (_, AnnLet bind body)
  = lvlBind ctxt_lvl envs bind		`thenLvl` \ (binds', new_envs) ->
    lvlExpr ctxt_lvl new_envs body	`thenLvl` \ body' ->
    returnLvl (foldr Let body' binds') -- mkCoLet* requires Core...

lvlExpr ctxt_lvl envs@(venv, tenv) (_, AnnCase expr alts)
  = lvlMFE ctxt_lvl envs expr	`thenLvl` \ expr' ->
    lvl_alts alts		`thenLvl` \ alts' ->
    returnLvl (Case expr' alts')
    where
      expr_type = coreExprType (deAnnotate expr)
      incd_lvl  = incMinorLvl ctxt_lvl

      lvl_alts (AnnAlgAlts alts deflt)
	= mapLvl lvl_alt alts	`thenLvl` \ alts' ->
	  lvl_deflt deflt	`thenLvl` \ deflt' ->
	  returnLvl (AlgAlts alts' deflt')
	where
	  lvl_alt (con, bs, e)
	    = let
		  bs'  = [ (b, incd_lvl) | b <- bs ]
		  new_envs = (growIdEnvList venv bs', tenv)
	      in
	      lvlMFE incd_lvl new_envs e	`thenLvl` \ e' ->
	      returnLvl (con, bs', e')

      lvl_alts (AnnPrimAlts alts deflt)
	= mapLvl lvl_alt alts	`thenLvl` \ alts' ->
	  lvl_deflt deflt	`thenLvl` \ deflt' ->
	  returnLvl (PrimAlts alts' deflt')
	where
	  lvl_alt (lit, e)
	    = lvlMFE incd_lvl envs e `thenLvl` \ e' ->
	      returnLvl (lit, e')

      lvl_deflt AnnNoDefault = returnLvl NoDefault

      lvl_deflt (AnnBindDefault b expr)
	= let
	      new_envs = (addOneToIdEnv venv b incd_lvl, tenv)
	  in
	  lvlMFE incd_lvl new_envs expr `thenLvl` \ expr' ->
	  returnLvl (BindDefault (b, incd_lvl) expr')
\end{code}

@lvlMFE@ is just like @lvlExpr@, except that it might let-bind
the expression, so that it can itself be floated.

\begin{code}
lvlMFE ::  Level		-- Level of innermost enclosing lambda/tylam
	-> LevelEnvs		-- Level of in-scope names/tyvars
	-> CoreExprWithFVs	-- input expression
	-> LvlM LevelledExpr	-- Result expression

lvlMFE ctxt_lvl envs@(venv,_) ann_expr
  | isPrimType ty	-- Can't let-bind it
  = lvlExpr ctxt_lvl envs ann_expr

  | otherwise		-- Not primitive type so could be let-bound
  = setFloatLevel False {- Not already let-bound -}
	ctxt_lvl envs ann_expr ty	`thenLvl` \ (final_lvl, expr') ->
    returnLvl expr'
  where
    ty = coreExprType (deAnnotate ann_expr)
\end{code}


%************************************************************************
%*									*
\subsection{Deciding floatability}
%*									*
%************************************************************************

@setFloatLevel@ is used for let-bound right-hand-sides, or for MFEs which
are being created as let-bindings

Decision tree:
Let Bound?
  YES. -> (a) try abstracting type variables.
       If we abstract type variables it will go further, that is, past more
       lambdas. same as asking if the level number given by the free
       variables is less than the level number given by free variables
       and type variables together.
       Abstract offending type variables, e.g.
       change f ty a b
       to let v = /\ty' -> f ty' a b
	  in v ty
       so that v' is not stopped by the level number of ty
       tag the original let with its level number
       (from its variables and type variables)
  NO.  is a WHNF?
	 YES. -> No point in let binding to float a WHNF.
		 Pin (leave) expression here.
	 NO. -> Will float past a lambda?
		(check using free variables only, not type variables)
		  YES. -> do the same as (a) above.
		  NO. -> No point in let binding if it is not going anywhere
			 Pin (leave) expression here.

\begin{code}
setFloatLevel :: Bool			-- True <=> the expression is already let-bound
					-- False <=> it's a possible MFE
	      -> Level 			-- of context
	      -> LevelEnvs

	      -> CoreExprWithFVs	-- Original rhs
	      -> Type 		-- Type of rhs

	      -> LvlM (Level, 		-- Level to attribute to this let-binding
		       LevelledExpr)	-- Final rhs

setFloatLevel alreadyLetBound ctxt_lvl envs@(venv, tenv)
	      expr@(FVInfo fvs tfvs might_leak, _) ty
-- Invariant: ctxt_lvl is never = Top
-- Beautiful ASSERT, dudes (WDP 95/04)...

-- Now deal with (by not floating) trivial non-let-bound expressions
-- which just aren't worth let-binding in order to float.  We always
-- choose to float even trivial let-bound things because it doesn't do
-- any harm, and not floating it may pin something important.  For
-- example
--
--	x = let v = []
--	        w = 1:v
--	    in ...
--
-- Here, if we don't float v we won't float w, which is Bad News.
-- If this gives any problems we could restrict the idea to things destined
-- for top level.

  | not alreadyLetBound
    && (manifestly_whnf || not will_float_past_lambda)
  =   -- Pin whnf non-let-bound expressions,
      -- or ones which aren't going anywhere useful
    lvlExpr ctxt_lvl envs expr        `thenLvl` \ expr' ->
    returnLvl (ctxt_lvl, expr')

  | alreadyLetBound && not worth_type_abstraction
  =   -- Process the expression with a new ctxt_lvl, obtained from
      -- the free vars of the expression itself
    lvlExpr (unTopify expr_lvl) envs expr `thenLvl` \ expr' ->
    returnLvl (maybe_unTopify expr_lvl, expr')

  | otherwise -- This will create a let anyway, even if there is no
	      -- type variable to abstract, so we try to abstract anyway
  = abstractWrtTyVars offending_tyvars ty envs lvl_after_ty_abstr expr
					      `thenLvl` \ final_expr ->
    returnLvl (expr_lvl, final_expr)
      -- OLD LIE: The body of the let, just a type application, isn't worth floating
      --          so pin it with ctxt_lvl
      -- The truth: better to give it expr_lvl in case it is pinning
      -- something non-trivial which depends on it.
  where
    fv_list = idSetToList    fvs
    tv_list = tyVarSetToList tfvs
    expr_lvl = ids_only_lvl `maxLvl` tyvars_only_lvl
    ids_only_lvl    = foldr (maxLvl . idLevel venv)    tOP_LEVEL fv_list
    tyvars_only_lvl = foldr (maxLvl . tyvarLevel tenv) tOP_LEVEL tv_list
    lvl_after_ty_abstr = ids_only_lvl --`maxLvl` non_offending_tyvars_lvl

    will_float_past_lambda = 	-- Will escape lambda if let-bound
			    ids_only_lvl `ltMajLvl` ctxt_lvl

    worth_type_abstraction = -- Will escape (more) lambda(s)/type lambda(s)
			     -- if type abstracted
      (ids_only_lvl `ltLvl` tyvars_only_lvl)
      && not (is_trivial de_ann_expr) -- avoids abstracting trivial type applications

    de_ann_expr = deAnnotate expr

    is_trivial (App e a)
      | notValArg a	= is_trivial e
    is_trivial (Var _)  = True
    is_trivial _        = False

    offending_tyvars = filter offending tv_list
    --non_offending_tyvars = filter (not . offending) tv_list
    --non_offending_tyvars_lvl = foldr (maxLvl . tyvarLevel tenv) tOP_LEVEL non_offending_tyvars

    offending tyvar = ids_only_lvl `ltLvl` tyvarLevel tenv tyvar

    manifestly_whnf = whnfOrBottom de_ann_expr

    maybe_unTopify Top | not (canFloatToTop (ty, expr)) = Level 0 0
    maybe_unTopify lvl                                  = lvl
	{- ToDo [Andre]: the line above (maybe) should be Level 1 0,
	-- so that the let will not go past the *last* lambda if it can
	-- generate a space leak. If it is already in major level 0
	-- It won't do any harm to give it a Level 1 0.
	-- we should do the same test not only for things with level Top,
	-- but also for anything that gets a major level 0.
	   the problem is that
	   f = \a -> let x = [1..1000]
		     in zip a x
	   ==>
	   f = let x = [1..1000]
	       in \a -> zip a x
	   is just as bad as floating x to the top level.
	   Notice it would be OK in cases like
	   f = \a -> let x = [1..1000]
			 y = length x
		     in a + y
	   ==>
	   f = let x = [1..1000]
		   y = length x
	       in \a -> a + y
	   as x will be gc'd after y is updated.
	   [We did not hit any problems with the above (Level 0 0) code
	    in nofib benchmark]
	-}
\end{code}

Abstract wrt tyvars, by making it just as if we had seen

     let v = /\a1..an. E
     in v a1 ... an

instead of simply E. The idea is that v can be freely floated, since it
has no free type variables. Of course, if E has no free type
variables, then we just return E.

\begin{code}
abstractWrtTyVars offending_tyvars ty (venv,tenv) lvl expr
  = lvlExpr incd_lvl new_envs expr	`thenLvl` \ expr' ->
    newLvlVar poly_ty			`thenLvl` \ poly_var ->
    let
       poly_var_rhs     = mkTyLam offending_tyvars expr'
       poly_var_binding = NonRec (poly_var, lvl) poly_var_rhs
       poly_var_app     = mkTyApp (Var poly_var) (mkTyVarTys offending_tyvars)
       final_expr       = Let poly_var_binding poly_var_app -- mkCoLet* requires Core
    in
    returnLvl final_expr
  where
    poly_ty = mkForAllTys offending_tyvars ty

	-- These defns are just like those in the TyLam case of lvlExpr
    (incd_lvl, tyvar_lvls) = mapAccumL next (unTopify lvl) offending_tyvars

    next lvl tyvar = (lvl1, (tyvar,lvl1))
		     where lvl1 = incMinorLvl lvl

    new_tenv = growTyVarEnvList tenv tyvar_lvls
    new_envs = (venv, new_tenv)
\end{code}

Recursive definitions.  We want to transform

	letrec
	   x1 = e1
	   ...
	   xn = en
	in
	body

to

	letrec
	   x1' = /\ ab -> let D' in e1
	   ...
	   xn' = /\ ab -> let D' in en
	in
	let D in body

where ab are the tyvars pinning the defn further in than it
need be, and D  is a bunch of simple type applications:

		x1_cl = x1' ab
		...
		xn_cl = xn' ab

The "_cl" indicates that in D, the level numbers on the xi are the context level
number; type applications aren't worth floating.  The D' decls are
similar:

		x1_ll = x1' ab
		...
		xn_ll = xn' ab

but differ in their level numbers; here the ab are the newly-introduced
type lambdas.

\begin{code}
decideRecFloatLevel ctxt_lvl envs@(venv, tenv) ids rhss
  | isTopMajLvl ids_only_lvl   &&		-- Destination = top
    not (all canFloatToTop (zipEqual "decideRec" tys rhss)) -- Some can't float to top
  = 	-- Pin it here
    let
	ids_w_lvls = ids `zip` repeat ctxt_lvl
	new_envs   = (growIdEnvList venv ids_w_lvls, tenv)
    in
    mapLvl (lvlExpr ctxt_lvl new_envs) rhss	`thenLvl` \ rhss' ->
    returnLvl (ctxt_lvl, [], rhss')

{- OMITTED; see comments above near isWorthFloatingExpr

  | not (any (isWorthFloating True . deAnnotate) rhss)
  =  	-- Pin it here
    mapLvl (lvlExpr ctxt_lvl envs) rhss	`thenLvl` \ rhss' ->
    returnLvl (ctxt_lvl, [], rhss')

-}

  | ids_only_lvl `ltLvl` tyvars_only_lvl
  = 	-- Abstract wrt tyvars;
	-- offending_tyvars is definitely non-empty
	-- (I love the ASSERT to check this...  WDP 95/02)
    let
	-- These defns are just like those in the TyLam case of lvlExpr
       (incd_lvl, tyvar_lvls) = mapAccumL next (unTopify ids_only_lvl) offending_tyvars

       next lvl tyvar = (lvl1, (tyvar,lvl1))
		     where lvl1 = incMinorLvl lvl

       ids_w_incd_lvl = [(id,incd_lvl) | id <- ids]
       new_tenv	      = growTyVarEnvList tenv tyvar_lvls
       new_venv	      = growIdEnvList    venv ids_w_incd_lvl
       new_envs	      = (new_venv, new_tenv)
    in
    mapLvl (lvlExpr incd_lvl new_envs) rhss	`thenLvl` \ rhss' ->
    mapLvl newLvlVar poly_tys			`thenLvl` \ poly_vars ->
    let
	ids_w_poly_vars = zipEqual "decideRec2" ids poly_vars

		-- The "d_rhss" are the right-hand sides of "D" and "D'"
		-- in the documentation above
	d_rhss = [ mkTyApp (Var poly_var) offending_tyvar_tys | poly_var <- poly_vars]

		-- "local_binds" are "D'" in the documentation above
	local_binds = zipWithEqual "SetLevels" NonRec ids_w_incd_lvl d_rhss

	poly_var_rhss = [ mkTyLam offending_tyvars (foldr Let rhs' local_binds)
			| rhs' <- rhss'	-- mkCoLet* requires Core...
			]

	poly_binds  = zipEqual "poly_binds" [(poly_var, ids_only_lvl) | poly_var <- poly_vars] poly_var_rhss

    in
    returnLvl (ctxt_lvl, [Rec poly_binds], d_rhss)
	-- The new right-hand sides, just a type application, aren't worth floating
	-- so pin it with ctxt_lvl

  | otherwise
  =	-- Let it float freely
    let
	ids_w_lvls = ids `zip` repeat expr_lvl
	new_envs   = (growIdEnvList venv ids_w_lvls, tenv)
    in
    mapLvl (lvlExpr (unTopify expr_lvl) new_envs) rhss	`thenLvl` \ rhss' ->
    returnLvl (expr_lvl, [], rhss')

  where
    tys  = map idType ids

    fvs  = unionManyIdSets [freeVarsOf   rhs | rhs <- rhss] `minusIdSet` mkIdSet ids
    tfvs = unionManyTyVarSets [freeTyVarsOf rhs | rhs <- rhss]
    fv_list = idSetToList fvs
    tv_list = tyVarSetToList tfvs

    ids_only_lvl    = foldr (maxLvl . idLevel venv)    tOP_LEVEL fv_list
    tyvars_only_lvl = foldr (maxLvl . tyvarLevel tenv) tOP_LEVEL tv_list
    expr_lvl 	    = ids_only_lvl `maxLvl` tyvars_only_lvl

    offending_tyvars
	| ids_only_lvl `ltLvl` tyvars_only_lvl = filter offending tv_list
	| otherwise 			       = []

    offending_tyvar_tys = mkTyVarTys offending_tyvars
    poly_tys = map (mkForAllTys offending_tyvars) tys

    offending tyvar = ids_only_lvl `ltLvl` tyvarLevel tenv tyvar
\end{code}


\begin{code}
{- ******** OMITTED NOW

isWorthFloating :: Bool		-- True <=> already let-bound
		-> CoreExpr	-- The expression
		-> Bool

isWorthFloating alreadyLetBound expr

  | alreadyLetBound = isWorthFloatingExpr expr

  | otherwise       = 	-- No point in adding a fresh let-binding for a WHNF, because
			-- floating it isn't beneficial enough.
		      isWorthFloatingExpr expr &&
		      not (whnfOrBottom expr)
********** -}

isWorthFloatingExpr :: CoreExpr -> Bool

isWorthFloatingExpr (Var v)	= False
isWorthFloatingExpr (Lit lit)	= False
isWorthFloatingExpr (App e arg)
  | notValArg arg		= isWorthFloatingExpr e
isWorthFloatingExpr (Con con as)
  | all notValArg as		= False	-- Just a type application
isWorthFloatingExpr _		= True

canFloatToTop :: (Type, CoreExprWithFVs) -> Bool

canFloatToTop (ty, (FVInfo _ _ (LeakFree _), expr)) = True
canFloatToTop (ty, (FVInfo _ _ MightLeak,    expr)) = isLeakFreeType [] ty

valSuggestsLeakFree expr = whnfOrBottom expr
\end{code}



%************************************************************************
%*									*
\subsection{Help functions}
%*									*
%************************************************************************

\begin{code}
idLevel :: IdEnv Level -> Id -> Level
idLevel venv v
  = case lookupIdEnv venv v of
      Just level -> level
      Nothing    -> tOP_LEVEL

tyvarLevel :: TyVarEnv Level -> TyVar -> Level
tyvarLevel tenv tyvar
  = case lookupTyVarEnv tenv tyvar of
      Just level -> level
      Nothing    -> tOP_LEVEL
\end{code}

\begin{code}
annCollectValBinders (_, (AnnLam (ValBinder arg) rhs))
  = (arg:args, body) 
  where
    (args, body) = annCollectValBinders rhs

annCollectValBinders body
  = ([], body)
\end{code}

%************************************************************************
%*									*
\subsection{Free-To-Level Monad}
%*									*
%************************************************************************

\begin{code}
type LvlM result = UniqSM result

thenLvl		= thenUs
returnLvl	= returnUs
mapLvl		= mapUs
mapAndUnzipLvl  = mapAndUnzipUs
mapAndUnzip3Lvl = mapAndUnzip3Us
\end{code}

We create a let-binding for `interesting' (non-utterly-trivial)
applications, to give them a fighting chance of being floated.

\begin{code}
newLvlVar :: Type -> LvlM Id

newLvlVar ty us
  = mkSysLocal SLIT("lvl") (getUnique us) ty noSrcLoc
\end{code}

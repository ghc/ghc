%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{SetLevels}

We attach binding levels to Core bindings, in preparation for floating
outwards (@FloatOut@).

We also let-ify many applications (notably case scrutinees), so they
will have a fighting chance of being floated sensible.

\begin{code}
module SetLevels (
	setLevels,

	Level(..), tOP_LEVEL,

	incMinorLvl, ltMajLvl, ltLvl, isTopLvl
    ) where

#include "HsVersions.h"

import CoreSyn

import CoreUtils	( coreExprType, exprIsTrivial, idFreeVars, exprIsBottom
			)
import FreeVars		-- all of it
import Id		( Id, idType, mkSysLocal )
import Var		( IdOrTyVar )
import VarEnv
import VarSet
import Type		( isUnLiftedType, mkTyVarTys, mkForAllTys, Type )
import VarSet
import VarEnv
import UniqSupply	( initUs, thenUs, returnUs, mapUs, mapAndUnzipUs, getUniqueUs,
			  mapAndUnzip3Us, UniqSM, UniqSupply )
import Maybes		( maybeToBool )
import Util		( zipWithEqual, zipEqual )
import Outputable

isLeakFreeType x y = False -- safe option; ToDo
\end{code}

%************************************************************************
%*									*
\subsection{Level numbers}
%*									*
%************************************************************************

\begin{code}
data Level
  = Top		-- Means *really* the top level; short for (Level 0 0).
  | Level   Int	-- Level number of enclosing lambdas
	    Int	-- Number of big-lambda and/or case expressions between
		-- here and the nearest enclosing lambda
\end{code}

The {\em level number} on a (type-)lambda-bound variable is the
nesting depth of the (type-)lambda which binds it.  The outermost lambda
has level 1, so (Level 0 0) means that the variable is bound outside any lambda.

On an expression, it's the maximum level number of its free
(type-)variables.  On a let(rec)-bound variable, it's the level of its
RHS.  On a case-bound variable, it's the number of enclosing lambdas.

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
type LevelledExpr  = TaggedExpr Level
type LevelledArg   = TaggedArg	Level
type LevelledBind  = TaggedBind Level

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

instance Outputable Level where
  ppr Top	      = ptext SLIT("<Top>")
  ppr (Level maj min) = hcat [ char '<', int maj, char ',', int min, char '>' ]
\end{code}

\begin{code}
type LevelEnv = VarEnv Level

varLevel :: LevelEnv -> IdOrTyVar -> Level
varLevel env v
  = case lookupVarEnv env v of
      Just level -> level
      Nothing    -> tOP_LEVEL

maxIdLvl :: LevelEnv -> IdOrTyVar -> Level -> Level
maxIdLvl env var lvl | isTyVar var = lvl
		     | otherwise   = case lookupVarEnv env var of
					Just lvl' -> maxLvl lvl' lvl
					Nothing   -> lvl 

maxTyVarLvl :: LevelEnv -> IdOrTyVar -> Level -> Level
maxTyVarLvl env var lvl | isId var  = lvl
		        | otherwise = case lookupVarEnv env var of
					Just lvl' -> maxLvl lvl' lvl
					Nothing   -> lvl 
\end{code}

%************************************************************************
%*									*
\subsection{Main level-setting code}
%*									*
%************************************************************************

\begin{code}
setLevels :: [CoreBind]
	  -> UniqSupply
	  -> [LevelledBind]

setLevels binds us
  = initLvl us (do_them binds)
  where
    -- "do_them"'s main business is to thread the monad along
    -- It gives each top binding the same empty envt, because
    -- things unbound in the envt have level number zero implicitly
    do_them :: [CoreBind] -> LvlM [LevelledBind]

    do_them [] = returnLvl []
    do_them (b:bs)
      = lvlTopBind b	`thenLvl` \ (lvld_bind, _) ->
	do_them bs	`thenLvl` \ lvld_binds ->
    	returnLvl (lvld_bind ++ lvld_binds)

initialEnv = emptyVarEnv

lvlTopBind (NonRec binder rhs)
  = lvlBind Top initialEnv (AnnNonRec binder (freeVars rhs))
					-- Rhs can have no free vars!

lvlTopBind (Rec pairs)
  = lvlBind Top initialEnv (AnnRec [(b,freeVars rhs) | (b,rhs) <- pairs])
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

The binding stuff works for top level too.

\begin{code}
lvlBind :: Level
	-> LevelEnv
	-> CoreBindWithFVs
	-> LvlM ([LevelledBind], LevelEnv)

lvlBind ctxt_lvl env (AnnNonRec name rhs)
  = setFloatLevel (Just name) ctxt_lvl env rhs ty 	`thenLvl` \ (final_lvl, rhs') ->
    let
	new_env = extendVarEnv env name final_lvl
    in
    returnLvl ([NonRec (name, final_lvl) rhs'], new_env)
  where
    ty = idType name


lvlBind ctxt_lvl env (AnnRec pairs)
  = decideRecFloatLevel ctxt_lvl env binders rhss	`thenLvl` \ (final_lvl, extra_binds, rhss') ->
    let
	binders_w_lvls = binders `zip` repeat final_lvl
	new_env        = extendVarEnvList env binders_w_lvls
    in
    returnLvl (extra_binds ++ [Rec (zipEqual "lvlBind" binders_w_lvls rhss')], new_env)
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
	-> LevelEnv		-- Level of in-scope names/tyvars
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
lvlExpr _ _ (_, AnnType ty) = returnLvl (Type ty)
lvlExpr _ _ (_, AnnVar v)   = returnLvl (Var v)

lvlExpr ctxt_lvl env (_, AnnCon con args)
  = mapLvl (lvlExpr ctxt_lvl env) args	`thenLvl` \ args' ->
    returnLvl (Con con args')

lvlExpr ctxt_lvl env (_, AnnApp fun arg)
  = lvlExpr ctxt_lvl env fun		`thenLvl` \ fun' ->
    lvlMFE  ctxt_lvl env arg		`thenLvl` \ arg' ->
    returnLvl (App fun' arg')

lvlExpr ctxt_lvl env (_, AnnNote note expr)
  = lvlExpr ctxt_lvl env expr 		`thenLvl` \ expr' ->
    returnLvl (Note note expr')

-- We don't split adjacent lambdas.  That is, given
--	\x y -> (x+1,y)
-- we don't float to give 
--	\x -> let v = x+y in \y -> (v,y)
-- Why not?  Because partial applications are fairly rare, and splitting
-- lambdas makes them more expensive.

lvlExpr ctxt_lvl env (_, AnnLam bndr rhs)
  = lvlMFE incd_lvl new_env body	`thenLvl` \ body' ->
    returnLvl (mkLams lvld_bndrs body')
  where
    bndr_is_id    = isId bndr
    bndr_is_tyvar = isTyVar bndr
    (bndrs, body) = go rhs

    incd_lvl   | bndr_is_id = incMajorLvl ctxt_lvl
	       | otherwise  = incMinorLvl ctxt_lvl
    lvld_bndrs = [(b,incd_lvl) | b <- (bndr:bndrs)]
    new_env    = extendVarEnvList env lvld_bndrs

    go (_, AnnLam bndr rhs) |  bndr_is_id && isId bndr 
			    || bndr_is_tyvar && isTyVar bndr
			    =  case go rhs of { (bndrs, body) -> (bndr:bndrs, body) }
    go body		    = ([], body)

lvlExpr ctxt_lvl env (_, AnnLet bind body)
  = lvlBind ctxt_lvl env bind		`thenLvl` \ (binds', new_env) ->
    lvlExpr ctxt_lvl new_env body	`thenLvl` \ body' ->
    returnLvl (mkLets binds' body')

lvlExpr ctxt_lvl env (_, AnnCase expr case_bndr alts)
  = lvlMFE ctxt_lvl env expr	`thenLvl` \ expr' ->
    mapLvl lvl_alt alts		`thenLvl` \ alts' ->
    returnLvl (Case expr' (case_bndr, incd_lvl) alts')
  where
      expr_type = coreExprType (deAnnotate expr)
      incd_lvl  = incMinorLvl ctxt_lvl
      alts_env  = extendVarEnv env case_bndr incd_lvl

      lvl_alt (con, bs, rhs)
        = let
		bs'  = [ (b, incd_lvl) | b <- bs ]
		new_env = extendVarEnvList alts_env bs'
          in
	  lvlMFE incd_lvl new_env rhs	`thenLvl` \ rhs' ->
	  returnLvl (con, bs', rhs')
\end{code}

@lvlMFE@ is just like @lvlExpr@, except that it might let-bind
the expression, so that it can itself be floated.

\begin{code}
lvlMFE ::  Level		-- Level of innermost enclosing lambda/tylam
	-> LevelEnv		-- Level of in-scope names/tyvars
	-> CoreExprWithFVs	-- input expression
	-> LvlM LevelledExpr	-- Result expression

lvlMFE ctxt_lvl env (_, AnnType ty)
  = returnLvl (Type ty)

lvlMFE ctxt_lvl env ann_expr
  | isUnLiftedType ty		-- Can't let-bind it
  = lvlExpr ctxt_lvl env ann_expr

  | otherwise		-- Not primitive type so could be let-bound
  = setFloatLevel Nothing {- Not already let-bound -}
	ctxt_lvl env ann_expr ty	`thenLvl` \ (final_lvl, expr') ->
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
setFloatLevel :: Maybe Id		-- Just id <=> the expression is already let-bound to id
					-- Nothing <=> it's a possible MFE
	      -> Level 			-- of context
	      -> LevelEnv

	      -> CoreExprWithFVs	-- Original rhs
	      -> Type 			-- Type of rhs

	      -> LvlM (Level, 		-- Level to attribute to this let-binding
		       LevelledExpr)	-- Final rhs

setFloatLevel maybe_let_bound ctxt_lvl env expr@(expr_fvs, _) ty

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
    && (expr_is_trivial || expr_is_bottom || not will_float_past_lambda)
  =   -- Pin trivial non-let-bound expressions,
      -- or ones which aren't going anywhere useful
    lvlExpr ctxt_lvl env expr		`thenLvl` \ expr' ->
    returnLvl (ctxt_lvl, expr')

{- SDM 7/98
The above case used to read (whnf_or_bottom || not will_float_past_lambda).  
It was changed because we really do want to float out constructors if possible:
this can save a great deal of needless allocation inside a loop.  On the other
hand, there's no point floating out nullary constructors and literals, hence
the expr_is_trivial condition.
-}

  | alreadyLetBound && not worth_type_abstraction
  =   -- Process the expression with a new ctxt_lvl, obtained from
      -- the free vars of the expression itself
    lvlExpr expr_lvl env expr		`thenLvl` \ expr' ->
    returnLvl (expr_lvl, expr')

  | otherwise -- This will create a let anyway, even if there is no
	      -- type variable to abstract, so we try to abstract anyway
  = abstractWrtTyVars offending_tyvars ty env lvl_after_ty_abstr expr
					      `thenLvl` \ final_expr ->
    returnLvl (expr_lvl, final_expr)
      -- OLD LIE: The body of the let, just a type application, isn't worth floating
      --          so pin it with ctxt_lvl
      -- The truth: better to give it expr_lvl in case it is pinning
      -- something non-trivial which depends on it.
  where
    alreadyLetBound = maybeToBool maybe_let_bound

    fvs 	       = case maybe_let_bound of
				Nothing -> expr_fvs
				Just id -> expr_fvs `unionVarSet` idFreeVars id

    ids_only_lvl       = foldVarSet (maxIdLvl    env) tOP_LEVEL fvs
    tyvars_only_lvl    = foldVarSet (maxTyVarLvl env) tOP_LEVEL fvs
    expr_lvl           = ids_only_lvl `maxLvl` tyvars_only_lvl
    lvl_after_ty_abstr = ids_only_lvl --`maxLvl` non_offending_tyvars_lvl

 	-- Will escape lambda if let-bound
    will_float_past_lambda = ids_only_lvl `ltMajLvl` ctxt_lvl
			    
	 -- Will escape (more) lambda(s)/type lambda(s) if type abstracted
    worth_type_abstraction =  (ids_only_lvl `ltLvl` tyvars_only_lvl)
			   && not expr_is_trivial	 -- Avoids abstracting trivial type applications

    offending_tyvars = filter offending_tv (varSetElems fvs)
    offending_tv var | isId var  = False
		     | otherwise = ids_only_lvl `ltLvl` varLevel env var

    expr_is_trivial = exprIsTrivial de_ann_expr
    expr_is_bottom  = exprIsBottom  de_ann_expr
    de_ann_expr     = deAnnotate expr
\end{code}

Abstract wrt tyvars, by making it just as if we had seen

     let v = /\a1..an. E
     in v a1 ... an

instead of simply E. The idea is that v can be freely floated, since it
has no free type variables. Of course, if E has no free type
variables, then we just return E.

\begin{code}
abstractWrtTyVars offending_tyvars ty env lvl expr
  = lvlExpr incd_lvl new_env expr	`thenLvl` \ expr' ->
    newLvlVar poly_ty			`thenLvl` \ poly_var ->
    let
       poly_var_rhs     = mkLams tyvar_lvls expr'
       poly_var_binding = NonRec (poly_var, lvl) poly_var_rhs
       poly_var_app     = mkTyApps (Var poly_var) (mkTyVarTys offending_tyvars)
       final_expr       = Let poly_var_binding poly_var_app -- mkCoLet* requires Core
    in
    returnLvl final_expr
  where
    poly_ty = mkForAllTys offending_tyvars ty

	-- These defns are just like those in the TyLam case of lvlExpr
    incd_lvl   = incMinorLvl lvl
    tyvar_lvls = [(tv,incd_lvl) | tv <- offending_tyvars]
    new_env    = extendVarEnvList env tyvar_lvls
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
decideRecFloatLevel ctxt_lvl env ids rhss
  | ids_only_lvl `ltLvl` tyvars_only_lvl
  = 	-- Abstract wrt tyvars;
	-- offending_tyvars is definitely non-empty
	-- (I love the ASSERT to check this...  WDP 95/02)
    let
       incd_lvl     = incMinorLvl ids_only_lvl
       tyvars_w_lvl = [(var,incd_lvl) | var <- offending_tyvars]
       ids_w_lvl    = [(var,incd_lvl) | var <- ids]
       new_env	    = extendVarEnvList env (tyvars_w_lvl ++ ids_w_lvl)
    in
    mapLvl (lvlExpr incd_lvl new_env) rhss	`thenLvl` \ rhss' ->
    mapLvl newLvlVar poly_tys			`thenLvl` \ poly_vars ->
    let
	ids_w_poly_vars = zipEqual "decideRec2" ids poly_vars

		-- The "d_rhss" are the right-hand sides of "D" and "D'"
		-- in the documentation above
	d_rhss = [ mkTyApps (Var poly_var) offending_tyvar_tys | poly_var <- poly_vars]

		-- "local_binds" are "D'" in the documentation above
	local_binds = zipWithEqual "SetLevels" NonRec ids_w_lvl d_rhss

	poly_var_rhss = [ mkLams tyvars_w_lvl (mkLets local_binds rhs')
			| rhs' <- rhss'
			]

	poly_binds  = zipEqual "poly_binds" [(poly_var, ids_only_lvl) | poly_var <- poly_vars] 
					    poly_var_rhss

    in
    returnLvl (ctxt_lvl, [Rec poly_binds], d_rhss)
	-- The new right-hand sides, just a type application, aren't worth floating
	-- so pin it with ctxt_lvl

  | otherwise
  =	-- Let it float freely
    let
	ids_w_lvls = ids `zip` repeat expr_lvl
	new_env   = extendVarEnvList env ids_w_lvls
    in
    mapLvl (lvlExpr expr_lvl new_env) rhss	`thenLvl` \ rhss' ->
    returnLvl (expr_lvl, [], rhss')

  where
	-- Finding the free vars of the binding group is annoying
    bind_fvs	    = (unionVarSets (map fst rhss) `unionVarSet` unionVarSets (map idFreeVars ids))
		      `minusVarSet`
		      mkVarSet ids

    ids_only_lvl    = foldVarSet (maxIdLvl    env) tOP_LEVEL bind_fvs
    tyvars_only_lvl = foldVarSet (maxTyVarLvl env) tOP_LEVEL bind_fvs
    expr_lvl 	    = ids_only_lvl `maxLvl` tyvars_only_lvl

    offending_tyvars = filter offending_tv (varSetElems bind_fvs)
    offending_tv var | isId var  = False
		     | otherwise = ids_only_lvl `ltLvl` varLevel env var
    offending_tyvar_tys = mkTyVarTys offending_tyvars

    tys  		= map idType ids
    poly_tys		= map (mkForAllTys offending_tyvars) tys
\end{code}

%************************************************************************
%*									*
\subsection{Free-To-Level Monad}
%*									*
%************************************************************************

\begin{code}
type LvlM result = UniqSM result

initLvl		= initUs
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
newLvlVar ty = getUniqueUs	`thenLvl` \ uniq ->
	       returnUs (mkSysLocal SLIT("lvl") uniq ty)
\end{code}

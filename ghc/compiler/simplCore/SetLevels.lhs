%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{SetLevels}

		***************************
			Overview
		***************************

1. We attach binding levels to Core bindings, in preparation for floating
   outwards (@FloatOut@).

2. We also let-ify many expressions (notably case scrutinees), so they
   will have a fighting chance of being floated sensible.

3. We clone the binders of any floatable let-binding, so that when it is
   floated out it will be unique.  (This used to be done by the simplifier
   but the latter now only ensures that there's no shadowing; indeed, even 
   that may not be true.)

   NOTE: this can't be done using the uniqAway idea, because the variable
 	 must be unique in the whole program, not just its current scope,
	 because two variables in different scopes may float out to the
	 same top level place

   NOTE: Very tiresomely, we must apply this substitution to
	 the rules stored inside a variable too.

   We do *not* clone top-level bindings, because some of them must not change,
   but we *do* clone bindings that are heading for the top level

4. In the expression
	case x of wild { p -> ...wild... }
   we substitute x for wild in the RHS of the case alternatives:
	case x of wild { p -> ...x... }
   This means that a sub-expression involving x is not "trapped" inside the RHS.
   And it's not inconvenient because we already have a substitution.

  Note that this is EXACTLY BACKWARDS from the what the simplifier does.
  The simplifier tries to get rid of occurrences of x, in favour of wild,
  in the hope that there will only be one remaining occurrence of x, namely
  the scrutinee of the case, and we can inline it.  

\begin{code}
module SetLevels (
	setLevels, 

	Level(..), tOP_LEVEL,
	LevelledBind, LevelledExpr,

	incMinorLvl, ltMajLvl, ltLvl, isTopLvl, isInlineCtxt
    ) where

#include "HsVersions.h"

import CoreSyn

import DynFlags	( FloatOutSwitches(..) )
import CoreUtils	( exprType, exprIsTrivial, exprIsCheap, mkPiTypes )
import CoreFVs		-- all of it
import CoreSubst	( Subst, emptySubst, extendInScope, extendIdSubst,
			  cloneIdBndr, cloneRecIdBndrs )
import Id		( Id, idType, mkSysLocalUnencoded, 
			  isOneShotLambda, zapDemandIdInfo,
			  idSpecialisation, idWorkerInfo, setIdInfo
			)
import IdInfo		( workerExists, vanillaIdInfo, isEmptySpecInfo )
import Var		( Var )
import VarSet
import VarEnv
import Name		( getOccName )
import OccName		( occNameUserString )
import Type		( isUnLiftedType, Type )
import BasicTypes	( TopLevelFlag(..) )
import UniqSupply
import Util		( sortLe, isSingleton, count )
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
\subsection{Level numbers}
%*									*
%************************************************************************

\begin{code}
data Level = InlineCtxt	-- A level that's used only for
			-- the context parameter ctxt_lvl
	   | Level Int	-- Level number of enclosing lambdas
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

The main function @lvlExpr@ carries a ``context level'' (@ctxt_lvl@).
That's meant to be the level number of the enclosing binder in the
final (floated) program.  If the level number of a sub-expression is
less than that of the context, then it might be worth let-binding the
sub-expression so that it will indeed float.  

If you can float to level @Level 0 0@ worth doing so because then your
allocation becomes static instead of dynamic.  We always start with
context @Level 0 0@.  


InlineCtxt
~~~~~~~~~~
@InlineCtxt@ very similar to @Level 0 0@, but is used for one purpose:
to say "don't float anything out of here".  That's exactly what we
want for the body of an INLINE, where we don't want to float anything
out at all.  See notes with lvlMFE below.

But, check this out:

-- At one time I tried the effect of not float anything out of an InlineMe,
-- but it sometimes works badly.  For example, consider PrelArr.done.  It
-- has the form 	__inline (\d. e)
-- where e doesn't mention d.  If we float this to 
--	__inline (let x = e in \d. x)
-- things are bad.  The inliner doesn't even inline it because it doesn't look
-- like a head-normal form.  So it seems a lesser evil to let things float.
-- In SetLevels we do set the context to (Level 0 0) when we get to an InlineMe
-- which discourages floating out.

So the conclusion is: don't do any floating at all inside an InlineMe.
(In the above example, don't float the {x=e} out of the \d.)

One particular case is that of workers: we don't want to float the
call to the worker outside the wrapper, otherwise the worker might get
inlined into the floated expression, and an importing module won't see
the worker at all.

\begin{code}
type LevelledExpr  = TaggedExpr Level
type LevelledBind  = TaggedBind Level

tOP_LEVEL   = Level 0 0
iNLINE_CTXT = InlineCtxt

incMajorLvl :: Level -> Level
-- For InlineCtxt we ignore any inc's; we don't want
-- to do any floating at all; see notes above
incMajorLvl InlineCtxt		= InlineCtxt
incMajorLvl (Level major minor) = Level (major+1) 0

incMinorLvl :: Level -> Level
incMinorLvl InlineCtxt		= InlineCtxt
incMinorLvl (Level major minor) = Level major (minor+1)

maxLvl :: Level -> Level -> Level
maxLvl InlineCtxt l2  = l2
maxLvl l1  InlineCtxt = l1
maxLvl l1@(Level maj1 min1) l2@(Level maj2 min2)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise					   = l2

ltLvl :: Level -> Level -> Bool
ltLvl any_lvl	 InlineCtxt  = False
ltLvl InlineCtxt (Level _ _) = True
ltLvl (Level maj1 min1) (Level maj2 min2)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl any_lvl	InlineCtxt     = False
ltMajLvl InlineCtxt	(Level maj2 _) = 0 < maj2
ltMajLvl (Level maj1 _) (Level maj2 _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl (Level 0 0) = True
isTopLvl other	     = False

isInlineCtxt :: Level -> Bool
isInlineCtxt InlineCtxt = True
isInlineCtxt other	= False

instance Outputable Level where
  ppr InlineCtxt      = text "<INLINE>"
  ppr (Level maj min) = hcat [ char '<', int maj, char ',', int min, char '>' ]

instance Eq Level where
  InlineCtxt	    == InlineCtxt	 = True
  (Level maj1 min1) == (Level maj2 min2) = maj1==maj2 && min1==min2
  l1		    == l2		 = False
\end{code}


%************************************************************************
%*									*
\subsection{Main level-setting code}
%*									*
%************************************************************************

\begin{code}
setLevels :: FloatOutSwitches
	  -> [CoreBind]
	  -> UniqSupply
	  -> [LevelledBind]

setLevels float_lams binds us
  = initLvl us (do_them binds)
  where
    -- "do_them"'s main business is to thread the monad along
    -- It gives each top binding the same empty envt, because
    -- things unbound in the envt have level number zero implicitly
    do_them :: [CoreBind] -> LvlM [LevelledBind]

    do_them [] = returnLvl []
    do_them (b:bs)
      = lvlTopBind init_env b	`thenLvl` \ (lvld_bind, _) ->
	do_them bs		`thenLvl` \ lvld_binds ->
    	returnLvl (lvld_bind : lvld_binds)

    init_env = initialEnv float_lams

lvlTopBind env (NonRec binder rhs)
  = lvlBind TopLevel tOP_LEVEL env (AnnNonRec binder (freeVars rhs))
					-- Rhs can have no free vars!

lvlTopBind env (Rec pairs)
  = lvlBind TopLevel tOP_LEVEL env (AnnRec [(b,freeVars rhs) | (b,rhs) <- pairs])
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
binder.  Here's an example

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
lvlExpr _ _ (_, AnnType ty)   = returnLvl (Type ty)
lvlExpr _ env (_, AnnVar v)   = returnLvl (lookupVar env v)
lvlExpr _ env (_, AnnLit lit) = returnLvl (Lit lit)

lvlExpr ctxt_lvl env (_, AnnApp fun arg)
  = lvl_fun fun				`thenLvl` \ fun' ->
    lvlMFE  False ctxt_lvl env arg	`thenLvl` \ arg' ->
    returnLvl (App fun' arg')
  where
-- gaw 2004
    lvl_fun (_, AnnCase _ _ _ _) = lvlMFE True ctxt_lvl env fun
    lvl_fun other 	       = lvlExpr ctxt_lvl env fun
	-- We don't do MFE on partial applications generally,
	-- but we do if the function is big and hairy, like a case

lvlExpr ctxt_lvl env (_, AnnNote InlineMe expr)
-- Don't float anything out of an InlineMe; hence the iNLINE_CTXT
  = lvlExpr iNLINE_CTXT env expr 	`thenLvl` \ expr' ->
    returnLvl (Note InlineMe expr')

lvlExpr ctxt_lvl env (_, AnnNote note expr)
  = lvlExpr ctxt_lvl env expr 		`thenLvl` \ expr' ->
    returnLvl (Note note expr')

-- We don't split adjacent lambdas.  That is, given
--	\x y -> (x+1,y)
-- we don't float to give 
--	\x -> let v = x+y in \y -> (v,y)
-- Why not?  Because partial applications are fairly rare, and splitting
-- lambdas makes them more expensive.

lvlExpr ctxt_lvl env expr@(_, AnnLam bndr rhs)
  = lvlMFE True new_lvl new_env body	`thenLvl` \ new_body ->
    returnLvl (mkLams new_bndrs new_body)
  where 
    (bndrs, body)	 = collectAnnBndrs expr
    (new_lvl, new_bndrs) = lvlLamBndrs ctxt_lvl bndrs
    new_env 		 = extendLvlEnv env new_bndrs
	-- At one time we called a special verion of collectBinders,
	-- which ignored coercions, because we don't want to split
	-- a lambda like this (\x -> coerce t (\s -> ...))
	-- This used to happen quite a bit in state-transformer programs,
	-- but not nearly so much now non-recursive newtypes are transparent.
	-- [See SetLevels rev 1.50 for a version with this approach.]

lvlExpr ctxt_lvl env (_, AnnLet (AnnNonRec bndr rhs) body)
  | isUnLiftedType (idType bndr)
	-- Treat unlifted let-bindings (let x = b in e) just like (case b of x -> e)
	-- That is, leave it exactly where it is
	-- We used to float unlifted bindings too (e.g. to get a cheap primop
	-- outside a lambda (to see how, look at lvlBind in rev 1.58)
	-- but an unrelated change meant that these unlifed bindings
	-- could get to the top level which is bad.  And there's not much point;
	-- unlifted bindings are always cheap, and so hardly worth floating.
  = lvlExpr ctxt_lvl env rhs		`thenLvl` \ rhs' ->
    lvlExpr incd_lvl env' body		`thenLvl` \ body' ->
    returnLvl (Let (NonRec bndr' rhs') body')
  where
    incd_lvl = incMinorLvl ctxt_lvl
    bndr' = TB bndr incd_lvl
    env'  = extendLvlEnv env [bndr']

lvlExpr ctxt_lvl env (_, AnnLet bind body)
  = lvlBind NotTopLevel ctxt_lvl env bind	`thenLvl` \ (bind', new_env) ->
    lvlExpr ctxt_lvl new_env body		`thenLvl` \ body' ->
    returnLvl (Let bind' body')

lvlExpr ctxt_lvl env (_, AnnCase expr case_bndr ty alts)
  = lvlMFE True ctxt_lvl env expr	`thenLvl` \ expr' ->
    let
	alts_env = extendCaseBndrLvlEnv env expr' case_bndr incd_lvl
    in
    mapLvl (lvl_alt alts_env) alts	`thenLvl` \ alts' ->
    returnLvl (Case expr' (TB case_bndr incd_lvl) ty alts')
  where
      incd_lvl  = incMinorLvl ctxt_lvl

      lvl_alt alts_env (con, bs, rhs)
	= lvlMFE True incd_lvl new_env rhs	`thenLvl` \ rhs' ->
	  returnLvl (con, bs', rhs')
	where
	  bs'     = [ TB b incd_lvl | b <- bs ]
	  new_env = extendLvlEnv alts_env bs'
\end{code}

@lvlMFE@ is just like @lvlExpr@, except that it might let-bind
the expression, so that it can itself be floated.

[NOTE: unlifted MFEs]
We don't float unlifted MFEs, which potentially loses big opportunites.
For example:
	\x -> f (h y)
where h :: Int -> Int# is expensive. We'd like to float the (h y) outside
the \x, but we don't because it's unboxed.  Possible solution: box it.

\begin{code}
lvlMFE ::  Bool			-- True <=> strict context [body of case or let]
	-> Level		-- Level of innermost enclosing lambda/tylam
	-> LevelEnv		-- Level of in-scope names/tyvars
	-> CoreExprWithFVs	-- input expression
	-> LvlM LevelledExpr	-- Result expression

lvlMFE strict_ctxt ctxt_lvl env (_, AnnType ty)
  = returnLvl (Type ty)


lvlMFE strict_ctxt ctxt_lvl env ann_expr@(fvs, _)
  |  isUnLiftedType ty			-- Can't let-bind it; see [NOTE: unlifted MFEs]
  || isInlineCtxt ctxt_lvl		-- Don't float out of an __inline__ context
  || exprIsTrivial expr			-- Never float if it's trivial
  || not good_destination
  = 	-- Don't float it out
    lvlExpr ctxt_lvl env ann_expr

  | otherwise	-- Float it out!
  = lvlFloatRhs abs_vars dest_lvl env ann_expr	`thenLvl` \ expr' ->
    newLvlVar "lvl" abs_vars ty			`thenLvl` \ var ->
    returnLvl (Let (NonRec (TB var dest_lvl) expr') 
		   (mkVarApps (Var var) abs_vars))
  where
    expr     = deAnnotate ann_expr
    ty       = exprType expr
    dest_lvl = destLevel env fvs (isFunction ann_expr)
    abs_vars = abstractVars dest_lvl env fvs

	-- A decision to float entails let-binding this thing, and we only do 
	-- that if we'll escape a value lambda, or will go to the top level.
    good_destination 
	| dest_lvl `ltMajLvl` ctxt_lvl		-- Escapes a value lambda
	= not (exprIsCheap expr) || isTopLvl dest_lvl
	  -- Even if it escapes a value lambda, we only
	  -- float if it's not cheap (unless it'll get all the
	  -- way to the top).  I've seen cases where we
	  -- float dozens of tiny free expressions, which cost
	  -- more to allocate than to evaluate.
	  -- NB: exprIsCheap is also true of bottom expressions, which
	  --     is good; we don't want to share them
	  --
	  -- It's only Really Bad to float a cheap expression out of a
	  -- strict context, because that builds a thunk that otherwise
	  -- would never be built.  So another alternative would be to
	  -- add 
	  -- 	|| (strict_ctxt && not (exprIsBottom expr))
	  -- to the condition above. We should really try this out.

	| otherwise		-- Does not escape a value lambda
	= isTopLvl dest_lvl 	-- Only float if we are going to the top level
	&& floatConsts env	--   and the floatConsts flag is on
	&& not strict_ctxt	-- Don't float from a strict context	
	  -- We are keen to float something to the top level, even if it does not
	  -- escape a lambda, because then it needs no allocation.  But it's controlled
	  -- by a flag, because doing this too early loses opportunities for RULES
	  -- which (needless to say) are important in some nofib programs
	  -- (gcd is an example).
	  --
	  -- Beware:
	  --	concat = /\ a -> foldr ..a.. (++) []
	  -- was getting turned into
	  --	concat = /\ a -> lvl a
	  --	lvl    = /\ a -> foldr ..a.. (++) []
	  -- which is pretty stupid.  Hence the strict_ctxt test
\end{code}


%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

The binding stuff works for top level too.

\begin{code}
lvlBind :: TopLevelFlag		-- Used solely to decide whether to clone
	-> Level		-- Context level; might be Top even for bindings nested in the RHS
				-- of a top level binding
	-> LevelEnv
	-> CoreBindWithFVs
	-> LvlM (LevelledBind, LevelEnv)

lvlBind top_lvl ctxt_lvl env (AnnNonRec bndr rhs@(rhs_fvs,_))
  | isInlineCtxt ctxt_lvl		-- Don't do anything inside InlineMe
  = lvlExpr ctxt_lvl env rhs			`thenLvl` \ rhs' ->
    returnLvl (NonRec (TB bndr ctxt_lvl) rhs', env)

  | null abs_vars
  =	-- No type abstraction; clone existing binder
    lvlExpr dest_lvl env rhs			`thenLvl` \ rhs' ->
    cloneVar top_lvl env bndr ctxt_lvl dest_lvl	`thenLvl` \ (env', bndr') ->
    returnLvl (NonRec (TB bndr' dest_lvl) rhs', env') 

  | otherwise
  = -- Yes, type abstraction; create a new binder, extend substitution, etc
    lvlFloatRhs abs_vars dest_lvl env rhs	`thenLvl` \ rhs' ->
    newPolyBndrs dest_lvl env abs_vars [bndr]	`thenLvl` \ (env', [bndr']) ->
    returnLvl (NonRec (TB bndr' dest_lvl) rhs', env')

  where
    bind_fvs = rhs_fvs `unionVarSet` idFreeVars bndr
    abs_vars = abstractVars dest_lvl env bind_fvs
    dest_lvl = destLevel env bind_fvs (isFunction rhs)
\end{code}


\begin{code}
lvlBind top_lvl ctxt_lvl env (AnnRec pairs)
  | isInlineCtxt ctxt_lvl	-- Don't do anything inside InlineMe
  = mapLvl (lvlExpr ctxt_lvl env) rhss			`thenLvl` \ rhss' ->
    returnLvl (Rec ([TB b ctxt_lvl | b <- bndrs] `zip` rhss'), env)

  | null abs_vars
  = cloneRecVars top_lvl env bndrs ctxt_lvl dest_lvl	`thenLvl` \ (new_env, new_bndrs) ->
    mapLvl (lvlExpr ctxt_lvl new_env) rhss		`thenLvl` \ new_rhss ->
    returnLvl (Rec ([TB b dest_lvl | b <- new_bndrs] `zip` new_rhss), new_env)

  | isSingleton pairs && count isId abs_vars > 1
  = 	-- Special case for self recursion where there are
	-- several variables carried around: build a local loop:	
	--	poly_f = \abs_vars. \lam_vars . letrec f = \lam_vars. rhs in f lam_vars
	-- This just makes the closures a bit smaller.  If we don't do
	-- this, allocation rises significantly on some programs
	--
	-- We could elaborate it for the case where there are several
	-- mutually functions, but it's quite a bit more complicated
	-- 
	-- This all seems a bit ad hoc -- sigh
    let
	(bndr,rhs) = head pairs
	(rhs_lvl, abs_vars_w_lvls) = lvlLamBndrs dest_lvl abs_vars
	rhs_env = extendLvlEnv env abs_vars_w_lvls
    in
    cloneVar NotTopLevel rhs_env bndr rhs_lvl rhs_lvl	`thenLvl` \ (rhs_env', new_bndr) ->
    let
	(lam_bndrs, rhs_body)     = collectAnnBndrs rhs
        (body_lvl, new_lam_bndrs) = lvlLamBndrs rhs_lvl lam_bndrs
	body_env 		  = extendLvlEnv rhs_env' new_lam_bndrs
    in
    lvlExpr body_lvl body_env rhs_body		`thenLvl` \ new_rhs_body ->
    newPolyBndrs dest_lvl env abs_vars [bndr]	`thenLvl` \ (poly_env, [poly_bndr]) ->
    returnLvl (Rec [(TB poly_bndr dest_lvl, 
	       mkLams abs_vars_w_lvls $
	       mkLams new_lam_bndrs $
	       Let (Rec [(TB new_bndr rhs_lvl, mkLams new_lam_bndrs new_rhs_body)]) 
		   (mkVarApps (Var new_bndr) lam_bndrs))],
	       poly_env)

  | otherwise	-- Non-null abs_vars
  = newPolyBndrs dest_lvl env abs_vars bndrs		`thenLvl` \ (new_env, new_bndrs) ->
    mapLvl (lvlFloatRhs abs_vars dest_lvl new_env) rhss `thenLvl` \ new_rhss ->
    returnLvl (Rec ([TB b dest_lvl | b <- new_bndrs] `zip` new_rhss), new_env)

  where
    (bndrs,rhss) = unzip pairs

	-- Finding the free vars of the binding group is annoying
    bind_fvs	    = (unionVarSets [ idFreeVars bndr `unionVarSet` rhs_fvs
				    | (bndr, (rhs_fvs,_)) <- pairs])
		      `minusVarSet`
		      mkVarSet bndrs

    dest_lvl = destLevel env bind_fvs (all isFunction rhss)
    abs_vars = abstractVars dest_lvl env bind_fvs

----------------------------------------------------
-- Three help functons for the type-abstraction case

lvlFloatRhs abs_vars dest_lvl env rhs
  = lvlExpr rhs_lvl rhs_env rhs	`thenLvl` \ rhs' ->
    returnLvl (mkLams abs_vars_w_lvls rhs')
  where
    (rhs_lvl, abs_vars_w_lvls) = lvlLamBndrs dest_lvl abs_vars
    rhs_env = extendLvlEnv env abs_vars_w_lvls
\end{code}


%************************************************************************
%*									*
\subsection{Deciding floatability}
%*									*
%************************************************************************

\begin{code}
lvlLamBndrs :: Level -> [CoreBndr] -> (Level, [TaggedBndr Level])
-- Compute the levels for the binders of a lambda group
-- The binders returned are exactly the same as the ones passed,
-- but they are now paired with a level
lvlLamBndrs lvl [] 
  = (lvl, [])

lvlLamBndrs lvl bndrs
  = go  (incMinorLvl lvl)
	False 	-- Havn't bumped major level in this group
	[] bndrs
  where
    go old_lvl bumped_major rev_lvld_bndrs (bndr:bndrs)
	| isId bndr && 			-- Go to the next major level if this is a value binder,
	  not bumped_major && 		-- and we havn't already gone to the next level (one jump per group)
	  not (isOneShotLambda bndr)	-- and it isn't a one-shot lambda
	= go new_lvl True (TB bndr new_lvl : rev_lvld_bndrs) bndrs

	| otherwise
	= go old_lvl bumped_major (TB bndr old_lvl : rev_lvld_bndrs) bndrs

	where
	  new_lvl = incMajorLvl old_lvl

    go old_lvl _ rev_lvld_bndrs []
	= (old_lvl, reverse rev_lvld_bndrs)
	-- a lambda like this (\x -> coerce t (\s -> ...))
	-- This happens quite a bit in state-transformer programs
\end{code}

\begin{code}
  -- Destintion level is the max Id level of the expression
  -- (We'll abstract the type variables, if any.)
destLevel :: LevelEnv -> VarSet -> Bool -> Level
destLevel env fvs is_function
  |  floatLams env
  && is_function = tOP_LEVEL		-- Send functions to top level; see
					-- the comments with isFunction
  | otherwise    = maxIdLevel env fvs

isFunction :: CoreExprWithFVs -> Bool
-- The idea here is that we want to float *functions* to
-- the top level.  This saves no work, but 
--	(a) it can make the host function body a lot smaller, 
--		and hence inlinable.  
--	(b) it can also save allocation when the function is recursive:
--	    h = \x -> letrec f = \y -> ...f...y...x...
--		      in f x
--     becomes
--	    f = \x y -> ...(f x)...y...x...
--	    h = \x -> f x x
--     No allocation for f now.
-- We may only want to do this if there are sufficiently few free 
-- variables.  We certainly only want to do it for values, and not for
-- constructors.  So the simple thing is just to look for lambdas
isFunction (_, AnnLam b e) | isId b    = True
			   | otherwise = isFunction e
isFunction (_, AnnNote n e)            = isFunction e
isFunction other 		       = False
\end{code}


%************************************************************************
%*									*
\subsection{Free-To-Level Monad}
%*									*
%************************************************************************

\begin{code}
type LevelEnv = (FloatOutSwitches,
		 VarEnv Level, 			-- Domain is *post-cloned* TyVars and Ids
	         Subst, 			-- Domain is pre-cloned Ids; tracks the in-scope set
						-- 	so that subtitution is capture-avoiding
	         IdEnv ([Var], LevelledExpr))	-- Domain is pre-cloned Ids
	-- We clone let-bound variables so that they are still
	-- distinct when floated out; hence the SubstEnv/IdEnv.
        -- (see point 3 of the module overview comment).
	-- We also use these envs when making a variable polymorphic
	-- because we want to float it out past a big lambda.
	--
	-- The SubstEnv and IdEnv always implement the same mapping, but the
	-- SubstEnv maps to CoreExpr and the IdEnv to LevelledExpr
	-- Since the range is always a variable or type application,
	-- there is never any difference between the two, but sadly
	-- the types differ.  The SubstEnv is used when substituting in
	-- a variable's IdInfo; the IdEnv when we find a Var.
	--
	-- In addition the IdEnv records a list of tyvars free in the
	-- type application, just so we don't have to call freeVars on
	-- the type application repeatedly.
	--
	-- The domain of the both envs is *pre-cloned* Ids, though
	--
	-- The domain of the VarEnv Level is the *post-cloned* Ids

initialEnv :: FloatOutSwitches -> LevelEnv
initialEnv float_lams = (float_lams, emptyVarEnv, emptySubst, emptyVarEnv)

floatLams :: LevelEnv -> Bool
floatLams (FloatOutSw float_lams _, _, _, _) = float_lams

floatConsts :: LevelEnv -> Bool
floatConsts (FloatOutSw _ float_consts, _, _, _) = float_consts

extendLvlEnv :: LevelEnv -> [TaggedBndr Level] -> LevelEnv
-- Used when *not* cloning
extendLvlEnv (float_lams, lvl_env, subst, id_env) prs
  = (float_lams,
     foldl add_lvl lvl_env prs,
     foldl del_subst subst prs,
     foldl del_id id_env prs)
  where
    add_lvl   env (TB v l) = extendVarEnv env v l
    del_subst env (TB v _) = extendInScope env v
    del_id    env (TB v _) = delVarEnv env v
  -- We must remove any clone for this variable name in case of
  -- shadowing.  This bit me in the following case
  -- (in nofib/real/gg/Spark.hs):
  -- 
  --   case ds of wild {
  --     ... -> case e of wild {
  --              ... -> ... wild ...
  --            }
  --   }
  -- 
  -- The inside occurrence of @wild@ was being replaced with @ds@,
  -- incorrectly, because the SubstEnv was still lying around.  Ouch!
  -- KSW 2000-07.

-- extendCaseBndrLvlEnv adds the mapping case-bndr->scrut-var if it can
-- (see point 4 of the module overview comment)
extendCaseBndrLvlEnv (float_lams, lvl_env, subst, id_env) (Var scrut_var) case_bndr lvl
  = (float_lams,
     extendVarEnv lvl_env case_bndr lvl,
     extendIdSubst subst case_bndr (Var scrut_var),
     extendVarEnv id_env case_bndr ([scrut_var], Var scrut_var))
     
extendCaseBndrLvlEnv env scrut case_bndr lvl
  = extendLvlEnv          env [TB case_bndr lvl]

extendPolyLvlEnv dest_lvl (float_lams, lvl_env, subst, id_env) abs_vars bndr_pairs
  = (float_lams,
     foldl add_lvl   lvl_env bndr_pairs,
     foldl add_subst subst   bndr_pairs,
     foldl add_id    id_env  bndr_pairs)
  where
     add_lvl   env (v,v') = extendVarEnv env v' dest_lvl
     add_subst env (v,v') = extendIdSubst env v (mkVarApps (Var v') abs_vars)
     add_id    env (v,v') = extendVarEnv env v ((v':abs_vars), mkVarApps (Var v') abs_vars)

extendCloneLvlEnv lvl (float_lams, lvl_env, _, id_env) new_subst bndr_pairs
  = (float_lams,
     foldl add_lvl   lvl_env bndr_pairs,
     new_subst,
     foldl add_id    id_env  bndr_pairs)
  where
     add_lvl   env (v,v') = extendVarEnv env v' lvl
     add_id    env (v,v') = extendVarEnv env v ([v'], Var v')


maxIdLevel :: LevelEnv -> VarSet -> Level
maxIdLevel (_, lvl_env,_,id_env) var_set
  = foldVarSet max_in tOP_LEVEL var_set
  where
    max_in in_var lvl = foldr max_out lvl (case lookupVarEnv id_env in_var of
						Just (abs_vars, _) -> abs_vars
						Nothing		   -> [in_var])

    max_out out_var lvl 
	| isId out_var = case lookupVarEnv lvl_env out_var of
				Just lvl' -> maxLvl lvl' lvl
				Nothing   -> lvl 
	| otherwise    = lvl	-- Ignore tyvars in *maxIdLevel*

lookupVar :: LevelEnv -> Id -> LevelledExpr
lookupVar (_, _, _, id_env) v = case lookupVarEnv id_env v of
				       Just (_, expr) -> expr
				       other	      -> Var v

abstractVars :: Level -> LevelEnv -> VarSet -> [Var]
	-- Find the variables in fvs, free vars of the target expresion,
	-- whose level is greater than the destination level
	-- These are the ones we are going to abstract out
abstractVars dest_lvl env fvs
  = uniq (sortLe le [var | fv <- varSetElems fvs, var <- absVarsOf dest_lvl env fv])
  where
	-- Sort the variables so we don't get 
	-- mixed-up tyvars and Ids; it's just messy
    v1 `le` v2 = case (isId v1, isId v2) of
		   (True, False) -> False
		   (False, True) -> True
		   other	 -> v1 <= v2	-- Same family

    uniq :: [Var] -> [Var]
	-- Remove adjacent duplicates; the sort will have brought them together
    uniq (v1:v2:vs) | v1 == v2  = uniq (v2:vs)
		    | otherwise = v1 : uniq (v2:vs)
    uniq vs = vs

absVarsOf :: Level -> LevelEnv -> Var -> [Var]
	-- If f is free in the expression, and f maps to poly_f a b c in the
	-- current substitution, then we must report a b c as candidate type
	-- variables
absVarsOf dest_lvl (_, lvl_env, _, id_env) v 
  | isId v
  = [zap av2 | av1 <- lookup_avs v, av2 <- add_tyvars av1, abstract_me av2]

  | otherwise
  = if abstract_me v then [v] else []

  where
    abstract_me v = case lookupVarEnv lvl_env v of
			Just lvl -> dest_lvl `ltLvl` lvl
			Nothing  -> False

    lookup_avs v = case lookupVarEnv id_env v of
			Just (abs_vars, _) -> abs_vars
			Nothing	           -> [v]

    add_tyvars v | isId v    = v : varSetElems (idFreeTyVars v)
		 | otherwise = [v]

	-- We are going to lambda-abstract, so nuke any IdInfo,
	-- and add the tyvars of the Id (if necessary)
    zap v | isId v = WARN( workerExists (idWorkerInfo v) ||
		           not (isEmptySpecInfo (idSpecialisation v)),
		           text "absVarsOf: discarding info on" <+> ppr v )
		     setIdInfo v vanillaIdInfo
	  | otherwise = v
\end{code}

\begin{code}
type LvlM result = UniqSM result

initLvl		= initUs_
thenLvl		= thenUs
returnLvl	= returnUs
mapLvl		= mapUs
\end{code}

\begin{code}
newPolyBndrs dest_lvl env abs_vars bndrs
  = getUniquesUs 		`thenLvl` \ uniqs ->
    let
	new_bndrs = zipWith mk_poly_bndr bndrs uniqs
    in
    returnLvl (extendPolyLvlEnv dest_lvl env abs_vars (bndrs `zip` new_bndrs), new_bndrs)
  where
    mk_poly_bndr bndr uniq = mkSysLocalUnencoded (mkFastString str) uniq poly_ty
			   where
			     str     = "poly_" ++ occNameUserString (getOccName bndr)
			     poly_ty = mkPiTypes abs_vars (idType bndr)
	

newLvlVar :: String 
	  -> [CoreBndr] -> Type 	-- Abstract wrt these bndrs
	  -> LvlM Id
newLvlVar str vars body_ty 	
  = getUniqueUs	`thenLvl` \ uniq ->
    returnUs (mkSysLocalUnencoded (mkFastString str) uniq (mkPiTypes vars body_ty))
    
-- The deeply tiresome thing is that we have to apply the substitution
-- to the rules inside each Id.  Grr.  But it matters.

cloneVar :: TopLevelFlag -> LevelEnv -> Id -> Level -> Level -> LvlM (LevelEnv, Id)
cloneVar TopLevel env v ctxt_lvl dest_lvl
  = returnUs (env, v)	-- Don't clone top level things
cloneVar NotTopLevel env@(_,_,subst,_) v ctxt_lvl dest_lvl
  = ASSERT( isId v )
    getUs	`thenLvl` \ us ->
    let
      (subst', v1) = cloneIdBndr subst us v
      v2	   = zap_demand ctxt_lvl dest_lvl v1
      env'	   = extendCloneLvlEnv dest_lvl env subst' [(v,v2)]
    in
    returnUs (env', v2)

cloneRecVars :: TopLevelFlag -> LevelEnv -> [Id] -> Level -> Level -> LvlM (LevelEnv, [Id])
cloneRecVars TopLevel env vs ctxt_lvl dest_lvl 
  = returnUs (env, vs)	-- Don't clone top level things
cloneRecVars NotTopLevel env@(_,_,subst,_) vs ctxt_lvl dest_lvl
  = ASSERT( all isId vs )
    getUs 			`thenLvl` \ us ->
    let
      (subst', vs1) = cloneRecIdBndrs subst us vs
      vs2	    = map (zap_demand ctxt_lvl dest_lvl) vs1
      env'	    = extendCloneLvlEnv dest_lvl env subst' (vs `zip` vs2)
    in
    returnUs (env', vs2)

	-- VERY IMPORTANT: we must zap the demand info 
	-- if the thing is going to float out past a lambda
zap_demand dest_lvl ctxt_lvl id
  | ctxt_lvl == dest_lvl = id			-- Stays put
  | otherwise		 = zapDemandIdInfo id	-- Floats out
\end{code}
	

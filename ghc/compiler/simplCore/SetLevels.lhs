%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{SetLevels}

		***************************
			Overview
		***************************

* We attach binding levels to Core bindings, in preparation for floating
  outwards (@FloatOut@).

* We also let-ify many expressions (notably case scrutinees), so they
  will have a fighting chance of being floated sensible.

* We clone the binders of any floatable let-binding, so that when it is
  floated out it will be unique.  (This used to be done by the simplifier
  but the latter now only ensures that there's no shadowing.)
  NOTE: Very tiresomely, we must apply this substitution to
	the rules stored inside a variable too.

  We do *not* clone top-level bindings, because some of them must not change,
  but we *do* clone bindings that are heading for the top level

* In the expression
	case x of wild { p -> ...wild... }
  we substitute x for wild in the RHS of the case alternatives:
	case x of wild { p -> ...x... }
  This means that a sub-expression involving x is not "trapped" inside the RHS.
  And it's not inconvenient because we already have a substitution.

\begin{code}
module SetLevels (
	setLevels,

	Level(..), tOP_LEVEL,

	incMinorLvl, ltMajLvl, ltLvl, isTopLvl
    ) where

#include "HsVersions.h"

import CoreSyn

import CoreUtils	( coreExprType, exprIsTrivial, exprIsBottom )
import CoreFVs		-- all of it
import Id		( Id, idType, mkSysLocal, isOneShotLambda, modifyIdInfo, 
			  getIdSpecialisation, getIdWorkerInfo
			)
import IdInfo		( workerExists )
import Var		( IdOrTyVar, Var, TyVar, setVarUnique )
import VarEnv
import Subst
import VarSet
import Name		( getOccName )
import OccName		( occNameUserString )
import Type		( isUnLiftedType, mkTyVarTy, mkForAllTys, Type )
import BasicTypes	( TopLevelFlag(..) )
import VarSet
import VarEnv
import UniqSupply
import Maybes		( maybeToBool )
import Util		( zipWithEqual, zipEqual )
import Outputable
import List		( nub )
\end{code}

%************************************************************************
%*									*
\subsection{Level numbers}
%*									*
%************************************************************************

\begin{code}
data Level = Level Int	-- Level number of enclosing lambdas
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
sub-expression so that it will indeed float. This context level starts
at @Level 0 0@.

\begin{code}
type LevelledExpr  = TaggedExpr Level
type LevelledArg   = TaggedArg	Level
type LevelledBind  = TaggedBind Level

tOP_LEVEL = Level 0 0

incMajorLvl :: Level -> Level
incMajorLvl (Level major minor) = Level (major+1) 0

incMinorLvl :: Level -> Level
incMinorLvl (Level major minor) = Level major (minor+1)

maxLvl :: Level -> Level -> Level
maxLvl l1@(Level maj1 min1) l2@(Level maj2 min2)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise					   = l2

ltLvl :: Level -> Level -> Bool
ltLvl (Level maj1 min1) (Level maj2 min2)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl (Level maj1 _) (Level maj2 _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl (Level 0 0) = True
isTopLvl other       = False

instance Outputable Level where
  ppr (Level maj min) = hcat [ char '<', int maj, char ',', int min, char '>' ]
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
    	returnLvl (lvld_bind : lvld_binds)

lvlTopBind (NonRec binder rhs)
  = lvlBind TopLevel tOP_LEVEL initialEnv (AnnNonRec binder (freeVars rhs))
					-- Rhs can have no free vars!

lvlTopBind (Rec pairs)
  = lvlBind TopLevel tOP_LEVEL initialEnv (AnnRec [(b,freeVars rhs) | (b,rhs) <- pairs])
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
lvlExpr _ _ (_, AnnType ty) = returnLvl (Type ty)
lvlExpr _ env (_, AnnVar v) = returnLvl (lookupVar env v)

lvlExpr ctxt_lvl env (_, AnnCon con args)
  = mapLvl (lvlExpr ctxt_lvl env) args	`thenLvl` \ args' ->
    returnLvl (Con con args')

lvlExpr ctxt_lvl env (_, AnnApp fun arg)
  = lvlExpr ctxt_lvl env fun		`thenLvl` \ fun' ->
    lvlMFE  False ctxt_lvl env arg	`thenLvl` \ arg' ->
    returnLvl (App fun' arg')

lvlExpr ctxt_lvl env (_, AnnNote InlineMe expr)
	-- Don't float anything out of an InlineMe
  = lvlExpr tOP_LEVEL env expr 		`thenLvl` \ expr' ->
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
  = go (incMinorLvl ctxt_lvl) env False {- Havn't bumped major level in this group -} expr
  where 
    go lvl env bumped_major (_, AnnLam bndr body)
      = go new_lvl new_env new_bumped_major body	`thenLvl` \ new_body ->
	returnLvl (Lam lvld_bndr new_body)
      where
	-- Go to the next major level if this is a value binder,
	-- and we havn't already gone to the next level (one jump per group)
	-- and it isn't a one-shot lambda
	(new_lvl, new_bumped_major)	
	  | isId bndr && 
	    not bumped_major && 
	    not (isOneShotLambda bndr) = (incMajorLvl ctxt_lvl, True)
	  | otherwise		       = (lvl, 		       bumped_major)
	new_env   = extendLvlEnv env [lvld_bndr]
	lvld_bndr = (bndr, new_lvl)

	-- Ignore notes, because we don't want to split
	-- a lambda like this (\x -> coerce t (\s -> ...))
	-- This happens quite a bit in state-transformer programs
    go lvl env bumped_major (_, AnnNote note body)
      = go lvl env bumped_major body			`thenLvl` \ new_body ->
	returnLvl (Note note new_body)

    go lvl env bumped_major body
      = lvlMFE True lvl env body


lvlExpr ctxt_lvl env (_, AnnLet bind body)
  = lvlBind NotTopLevel ctxt_lvl env bind	`thenLvl` \ (bind', new_env) ->
    lvlExpr ctxt_lvl new_env body		`thenLvl` \ body' ->
    returnLvl (Let bind' body')

lvlExpr ctxt_lvl env (_, AnnCase expr case_bndr alts)
  = lvlMFE True ctxt_lvl env expr	`thenLvl` \ expr' ->
    let
	alts_env = extendCaseBndrLvlEnv env expr' case_bndr incd_lvl
    in
    mapLvl (lvl_alt alts_env) alts	`thenLvl` \ alts' ->
    returnLvl (Case expr' (case_bndr, incd_lvl) alts')
  where
      expr_type = coreExprType (deAnnotate expr)
      incd_lvl  = incMinorLvl ctxt_lvl

      lvl_alt alts_env (con, bs, rhs)
	= lvlMFE True incd_lvl new_env rhs	`thenLvl` \ rhs' ->
	  returnLvl (con, bs', rhs')
	where
	  bs'     = [ (b, incd_lvl) | b <- bs ]
	  new_env = extendLvlEnv alts_env bs'
\end{code}

@lvlMFE@ is just like @lvlExpr@, except that it might let-bind
the expression, so that it can itself be floated.

\begin{code}
lvlMFE ::  Bool			-- True <=> strict context [body of case or let]
	-> Level		-- Level of innermost enclosing lambda/tylam
	-> LevelEnv		-- Level of in-scope names/tyvars
	-> CoreExprWithFVs	-- input expression
	-> LvlM LevelledExpr	-- Result expression

lvlMFE strict_ctxt ctxt_lvl env (_, AnnType ty)
  = returnLvl (Type ty)

lvlMFE strict_ctxt ctxt_lvl env ann_expr@(fvs, _)
  |  isUnLiftedType ty				-- Can't let-bind it
  || not (dest_lvl `ltMajLvl` ctxt_lvl)		-- Does not escape a value lambda
	-- A decision to float entails let-binding this thing, and we only do 
	-- that if we'll escape a value lambda.  I considered doing it if it
	-- would make the thing go to top level, but I found things like
	--	concat = /\ a -> foldr ..a.. (++) []
	-- was getting turned into
	--	concat = /\ a -> lvl a
	--	lvl    = /\ a -> foldr ..a.. (++) []
	-- which is pretty stupid.  So for now at least, I don't let-bind things
	-- simply because they could go to top level.
  || exprIsTrivial expr				-- Is trivial
  || (strict_ctxt && exprIsBottom expr)		-- Strict context and is bottom
  = 	-- Don't float it out
    lvlExpr ctxt_lvl env ann_expr

  | otherwise	-- Float it out!
  = lvlExpr expr_lvl expr_env ann_expr		`thenLvl` \ expr' ->
    newLvlVar "lvl" (mkForAllTys tyvars ty)	`thenLvl` \ var ->
    returnLvl (Let (NonRec (var,dest_lvl) (mkLams tyvars_w_lvls expr')) 
		   (mkTyVarApps var tyvars))
  where
    expr     = deAnnotate ann_expr
    ty       = coreExprType expr
    dest_lvl = destLevel env fvs
    (tyvars, tyvars_w_lvls, expr_lvl) = abstractTyVars dest_lvl env fvs
    expr_env = extendLvlEnv env tyvars_w_lvls
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
  | null tyvars
  =	-- No type abstraction; clone existing binder
    lvlExpr rhs_lvl rhs_env rhs			`thenLvl` \ rhs' ->
    cloneVar top_lvl env bndr dest_lvl		`thenLvl` \ (env', bndr') ->
    returnLvl (NonRec (bndr', dest_lvl) rhs', env') 

  | otherwise
  = -- Yes, type abstraction; create a new binder, extend substitution, etc
    WARN( workerExists (getIdWorkerInfo bndr)
	  || not (isEmptyCoreRules (getIdSpecialisation bndr)),
	  text "lvlBind: discarding info on" <+> ppr bndr )
	
    lvl_poly_rhs tyvars_w_lvls rhs_lvl rhs_env rhs	`thenLvl` \ rhs' ->
    new_poly_bndr tyvars bndr 				`thenLvl` \ bndr' ->
    let
	env' = extendPolyLvlEnv env dest_lvl tyvars [(bndr, bndr')]
    in
    returnLvl (NonRec (bndr', dest_lvl) rhs', env')

  where
    bind_fvs = rhs_fvs `unionVarSet` idFreeVars bndr

    dest_lvl | isUnLiftedType (idType bndr) = destLevel env bind_fvs `maxLvl` Level 1 0
	     | otherwise		    = destLevel env bind_fvs
	-- Hack alert!  We do have some unlifted bindings, for cheap primops, and 
	-- it is ok to float them out; but not to the top level.  If they would otherwise
	-- go to the top level, we pin them inside the topmost lambda

    (tyvars, tyvars_w_lvls, rhs_lvl) = abstractTyVars dest_lvl env bind_fvs
    rhs_env = extendLvlEnv env tyvars_w_lvls
\end{code}


\begin{code}
lvlBind top_lvl ctxt_lvl env (AnnRec pairs)
  | null tyvars
  = cloneVars top_lvl env bndrs dest_lvl	`thenLvl` \ (new_env, new_bndrs) ->
    mapLvl (lvlExpr rhs_lvl new_env) rhss	`thenLvl` \ new_rhss ->
    returnLvl (Rec ((new_bndrs `zip` repeat dest_lvl) `zip` new_rhss), new_env)

  | otherwise
  = mapLvl (new_poly_bndr tyvars) bndrs		`thenLvl` \ new_bndrs ->
    let
	new_env = extendPolyLvlEnv env dest_lvl tyvars (bndrs `zip` new_bndrs)
 	rhs_env = extendLvlEnv new_env tyvars_w_lvls
   in
    mapLvl (lvl_poly_rhs tyvars_w_lvls rhs_lvl rhs_env) rhss 	`thenLvl` \ new_rhss ->
    returnLvl (Rec ((new_bndrs `zip` repeat dest_lvl) `zip` new_rhss), new_env)

  where
    (bndrs,rhss) = unzip pairs

	-- Finding the free vars of the binding group is annoying
    bind_fvs	    = (unionVarSets [ idFreeVars bndr `unionVarSet` rhs_fvs
				    | (bndr, (rhs_fvs,_)) <- pairs])
		      `minusVarSet`
		      mkVarSet bndrs

    dest_lvl	    = destLevel env bind_fvs

    (tyvars, tyvars_w_lvls, rhs_lvl) = abstractTyVars dest_lvl env bind_fvs

----------------------------------------------------
-- Three help functons Stuff for the type-abstraction case

new_poly_bndr tyvars bndr 
  = newLvlVar ("poly_" ++ occNameUserString (getOccName bndr))
	      (mkForAllTys tyvars (idType bndr))

lvl_poly_rhs tyvars_w_lvls rhs_lvl rhs_env rhs
 = lvlExpr rhs_lvl rhs_env rhs	`thenLvl` \ rhs' ->
   returnLvl (mkLams tyvars_w_lvls rhs')
\end{code}


%************************************************************************
%*									*
\subsection{Deciding floatability}
%*									*
%************************************************************************

\begin{code}
abstractTyVars :: Level -> LevelEnv -> VarSet
	       -> ([TyVar], [(TyVar,Level)], Level)
	-- Find the tyvars whose level is higher than the supplied level
	-- There should be no Ids with this property
abstractTyVars lvl env fvs
  | null tyvars = ([], [], lvl)		-- Don't increment level

  | otherwise
  = ASSERT( not (any bad fv_list) )
    (tyvars, tyvars_w_lvls, incd_lvl)
  where
    bad v   = isId v && lvl `ltLvl` varLevel env v
    fv_list = varSetElems fvs
    tyvars  = nub [tv | v <- fv_list, tv <- tvs_of v, abstract_tv tv]

	-- If f is free in the exression, and f maps to poly_f a b c in the
	-- current substitution, then we must report a b c as candidate type
	-- variables
    tvs_of v | isId v    = lookupTyVars env v
	     | otherwise = [v]

    abstract_tv var | isId var  = False
		    | otherwise = lvl `ltLvl` varLevel env var

	-- These defns are just like those in the TyLam case of lvlExpr
    incd_lvl      = incMinorLvl lvl
    tyvars_w_lvls = [(tv,incd_lvl) | tv <- tyvars]


  -- Destintion level is the max Id level of the expression
  -- (We'll abstract the type variables, if any.)
destLevel :: LevelEnv -> VarSet -> Level
destLevel env fvs = foldVarSet (maxIdLvl env) tOP_LEVEL fvs

maxIdLvl :: LevelEnv -> IdOrTyVar -> Level -> Level
maxIdLvl (lvl_env,_,_) var lvl | isTyVar var = lvl
		               | otherwise   = case lookupVarEnv lvl_env var of
						  Just lvl' -> maxLvl lvl' lvl
						  Nothing   -> lvl 
\end{code}


%************************************************************************
%*									*
\subsection{Free-To-Level Monad}
%*									*
%************************************************************************

\begin{code}
type LevelEnv = (VarEnv Level, SubstEnv, IdEnv ([TyVar], LevelledExpr))
	-- We clone let-bound variables so that they are still
	-- distinct when floated out; hence the SubstEnv/IdEnv.
	-- We also use these envs when making a variable polymorphic
	-- because we want to float it out past a big lambda.
	--
	-- The two Envs always implement the same mapping, but the
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

initialEnv :: LevelEnv
initialEnv = (emptyVarEnv, emptySubstEnv, emptyVarEnv)

extendLvlEnv :: LevelEnv -> [(Var,Level)] -> LevelEnv
	-- Used when *not* cloning
extendLvlEnv (lvl_env, subst_env, id_env) prs
  = (foldl add lvl_env prs, subst_env, id_env)
  where
    add env (v,l) = extendVarEnv env v l

-- extendCaseBndrLvlEnv adds the mapping case-bndr->scrut-var if it can
extendCaseBndrLvlEnv (lvl_env, subst_env, id_env) scrut case_bndr lvl
  = case scrut of
	Var v -> (new_lvl_env, extendSubstEnv subst_env case_bndr (DoneEx (Var v)), 
			       extendVarEnv   id_env    case_bndr ([], scrut))
	other -> (new_lvl_env, subst_env, id_env)
  where
    new_lvl_env = extendVarEnv lvl_env case_bndr lvl

extendPolyLvlEnv (lvl_env, subst_env, id_env) dest_lvl tyvars bndr_pairs
  = (foldl add_lvl lvl_env bndr_pairs,
     foldl add_subst subst_env bndr_pairs,
     foldl add_id    id_env    bndr_pairs)
  where
     add_lvl   env (v,_ ) = extendVarEnv   env v dest_lvl
     add_subst env (v,v') = extendSubstEnv env v (DoneEx (mkTyVarApps v' tyvars))
     add_id    env (v,v') = extendVarEnv   env v (tyvars, mkTyVarApps v' tyvars)

varLevel :: LevelEnv -> IdOrTyVar -> Level
varLevel (lvl_env, _, _) v
  = case lookupVarEnv lvl_env v of
      Just level -> level
      Nothing    -> tOP_LEVEL

lookupVar :: LevelEnv -> Id -> LevelledExpr
lookupVar (_, _, id_env) v = case lookupVarEnv id_env v of
			       Just (_, expr) -> expr
			       other	      -> Var v

lookupTyVars :: LevelEnv -> Id -> [TyVar]
lookupTyVars (_, _, id_env) v = case lookupVarEnv id_env v of
				  Just (tyvars, _) -> tyvars
				  Nothing	   -> []
\end{code}

\begin{code}
type LvlM result = UniqSM result

initLvl		= initUs_
thenLvl		= thenUs
returnLvl	= returnUs
mapLvl		= mapUs
\end{code}

\begin{code}
newLvlVar :: String -> Type -> LvlM Id
newLvlVar str ty = getUniqueUs	`thenLvl` \ uniq ->
	           returnUs (mkSysLocal (_PK_ str) uniq ty)

-- The deeply tiresome thing is that we have to apply the substitution
-- to the rules inside each Id.  Grr.  But it matters.

cloneVar :: TopLevelFlag -> LevelEnv -> Id -> Level -> LvlM (LevelEnv, Id)
cloneVar TopLevel env v lvl
  = returnUs (env, v)	-- Don't clone top level things
cloneVar NotTopLevel (lvl_env, subst_env, id_env) v lvl
  = getUniqueUs	`thenLvl` \ uniq ->
    let
      subst	 = mkSubst emptyVarSet subst_env
      v'	 = setVarUnique v uniq
      v''	 = modifyIdInfo (\info -> substIdInfo subst info info) v'
      subst_env' = extendSubstEnv subst_env v (DoneEx (Var v''))
      id_env'    = extendVarEnv   id_env v ([], Var v'')
      lvl_env'   = extendVarEnv   lvl_env v lvl
    in
    returnUs ((lvl_env', subst_env', id_env'), v'')

cloneVars :: TopLevelFlag -> LevelEnv -> [Id] -> Level -> LvlM (LevelEnv, [Id])
cloneVars TopLevel env vs lvl 
  = returnUs (env, vs)	-- Don't clone top level things
cloneVars NotTopLevel (lvl_env, subst_env, id_env) vs lvl
  = getUniquesUs (length vs)	`thenLvl` \ uniqs ->
    let
      subst	 = mkSubst emptyVarSet subst_env'
      vs'	 = zipWith setVarUnique vs uniqs
      vs''	 = map (modifyIdInfo (\info -> substIdInfo subst info info)) vs'
      subst_env' = extendSubstEnvList subst_env vs [DoneEx (Var v'') | v'' <- vs'']
      id_env'    = extendVarEnvList id_env (vs `zip` [([], Var v') | v' <- vs''])
      lvl_env'   = extendVarEnvList lvl_env (vs `zip` repeat lvl)
    in
    returnUs ((lvl_env', subst_env', id_env'), vs'')

mkTyVarApps var tyvars = foldl (\e tv -> App e (Type (mkTyVarTy tv))) 
			       (Var var) tyvars
\end{code}

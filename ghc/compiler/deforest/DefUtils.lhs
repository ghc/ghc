%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DefUtils]{Miscellaneous Utility functions}

>#include "HsVersions.h"

> module DefUtils (
> 	strip, stripAtom, stripCaseAlts, freeVars, renameExprs, rebindExpr,
>	atom2expr, newDefId, newTmpId, deforestable, foldrSUs,
>	mkDefLetrec, subst, freeTyVars, union, consistent, RenameResult(..),
>	isArgId
>	)
> 	where

> import DefSyn
> import Def2Core	-- tmp, for traces

>#ifdef __HBC__
> import Trace
>#endif

> import Type		( cloneTyVar, mkTyVarTy, applyTypeEnvToTy,
> 			  tyVarsOfType, TyVar, SYN_IE(SigmaType)
>			)
> import Literal	( Literal )	-- for Eq Literal
> import CoreSyn
> import Id		( mkIdWithNewUniq, mkSysLocal, applyTypeEnvToId,
> 			  getIdInfo, toplevelishId, idType, Id )
> import IdInfo
> import Outputable
> import Pretty
> import PrimOp	( PrimOp )	-- for Eq PrimOp
> import UniqSupply
> import SrcLoc		( mkUnknownSrcLoc )
> import Util

-----------------------------------------------------------------------------
\susbsection{Strip}

Implementation of the strip function.  Strip is the identity on
expressions (recursing into subterms), but replaces each label with
its left hand side.  The result is a term with no labels.

> strip :: DefExpr -> DefExpr

> strip e' = case e' of
> 	Var (DefArgExpr e) -> panic "DefUtils(strip): Var (DefExpr _)"
>	Var (Label l e)    -> l
>       Var (DefArgVar v)  -> e'
>       Lit l              -> e'
>       Con c ts es        -> Con c ts (map stripAtom es)
>       Prim op ts es      -> Prim op ts (map stripAtom es)
>       Lam vs e           -> Lam vs (strip e)
>       CoTyLam alpha e      -> CoTyLam alpha (strip e)
>       App e v            -> App (strip e) (stripAtom v)
>       CoTyApp e t          -> CoTyApp (strip e) t
>       Case e ps          -> Case (strip e) (stripCaseAlts ps)
>       Let (NonRec v e) e' -> Let (NonRec v (strip e)) (strip e')
>       Let (Rec bs) e   ->
>		Let (Rec [ (v, strip e) | (v,e) <- bs ]) (strip e)
>       SCC l e            -> SCC l (strip e)
>	Coerce _ _ _	   -> panic "DefUtils:strip:Coerce"

> stripAtom :: DefAtom -> DefAtom
> stripAtom (VarArg v) = VarArg (stripArg v)
> stripAtom (LitArg l) = LitArg l	-- XXX

> stripArg :: DefBindee -> DefBindee
> stripArg (DefArgExpr e) = DefArgExpr (strip e)
> stripArg (Label l e)   = panic "DefUtils(stripArg): Label _ _"
> stripArg (DefArgVar v) = panic "DefUtils(stripArg): DefArgVar _ _"

> stripCaseAlts (AlgAlts as def)
> 	= AlgAlts (map stripAlgAlt as) (stripDefault def)
> stripCaseAlts (PrimAlts as def)
> 	= PrimAlts (map stripPrimAlt as) (stripDefault def)

> stripAlgAlt  (c, vs, e) = (c, vs, strip e)
> stripPrimAlt (l, e) = (l, strip e)

> stripDefault NoDefault = NoDefault
> stripDefault (BindDefault v e) = BindDefault v (strip e)

-----------------------------------------------------------------------------
\subsection{Free Variables}

Find the free variables of an expression.  With labels, we descend
into the left side since this is the only sensible thing to do.
Strictly speaking, for a term (Label l e), freeVars l == freeVars e,
but l is guranteed to be finite so we choose that one.

> freeVars :: DefExpr -> [Id]
> freeVars e = free e []
>   where
>   	free e fvs = case e of
>		Var (DefArgExpr e) ->
>			panic "DefUtils(free): Var (DefExpr _)"
>		Var (Label l e)    -> free l fvs
>       	Var (DefArgVar v)
>			| v `is_elem` fvs	-> fvs
>			| otherwise	-> v : fvs
>		  where { is_elem = isIn "freeVars(deforest)" }
>       	Lit l              -> fvs
>       	Con c ts es        -> foldr freeAtom fvs es
>       	Prim op ts es      -> foldr freeAtom fvs es
>       	Lam vs e           -> free' vs (free e fvs)
>       	CoTyLam alpha e      -> free e fvs
>       	App 	e v          -> free e (freeAtom v fvs)
>       	CoTyApp e t          -> free e fvs
>       	Case e ps          -> free e (freeCaseAlts ps fvs)
>       	Let (NonRec v e) e' -> free e (free' [v] (free e' fvs))
>       	Let (Rec bs) e   -> free' vs (foldr free (free e fvs) es)
>			where (vs,es) = unzip bs
>       	SCC l e            -> free e fvs
>		Coerce _ _ _	   -> panic "DefUtils.freeVars:Coerce"

>	free' :: [Id] -> [Id] -> [Id]
> 	free' vs fvs = filter (\x -> notElem x vs) fvs

> 	freeAtom (VarArg (DefArgExpr e)) fvs = free e fvs
> 	freeAtom (VarArg (Label l e)) fvs
> 		= panic "DefUtils(free): VarArg (Label _ _)"
> 	freeAtom (VarArg (DefArgVar v)) fvs
> 		= panic "DefUtils(free): VarArg (DefArgVar _ _)"
> 	freeAtom (LitArg l) fvs = fvs

> 	freeCaseAlts (AlgAlts as def) fvs
> 		= foldr freeAlgAlt  (freeDefault def fvs) as
> 	freeCaseAlts (PrimAlts as def) fvs
> 		= foldr freePrimAlt (freeDefault def fvs) as
>
> 	freeAlgAlt  (c, vs, e) fvs = free' vs (free e fvs)
> 	freePrimAlt (l, e) fvs = free e fvs

> 	freeDefault NoDefault fvs = fvs
> 	freeDefault (BindDefault v e) fvs = free' [v] (free e fvs)

-----------------------------------------------------------------------------
\subsection{Free Type Variables}

> freeTyVars :: DefExpr -> [TyVar]
> freeTyVars e = free e []
>   where
>   	free e tvs = case e of
>		Var (DefArgExpr e)    ->
>			panic "DefUtils(freeVars): Var (DefExpr _)"
>		Var (Label l e)       -> free l tvs
>       	Var (DefArgVar id)    -> freeId id tvs
>       	Lit l                 -> tvs
>       	Con c ts es           -> foldr freeTy (foldr freeAtom tvs es) ts
>       	Prim op ts es         -> foldr freeTy (foldr freeAtom tvs es) ts
>       	Lam vs e              -> foldr freeId (free e tvs) vs
>       	CoTyLam alpha e         -> filter (/= alpha) (free e tvs)
>       	App e v               -> free e (freeAtom v tvs)
>       	CoTyApp e t             -> free e (freeTy t tvs)
>       	Case e ps             -> free e (freeCaseAlts ps tvs)
>       	Let (NonRec v e) e' -> free e (freeId v (free e' tvs))
>       	Let (Rec bs) e      -> foldr freeBind (free e tvs) bs
>       	SCC l e               -> free e tvs
>		Coerce _ _ _	      -> panic "DefUtils.freeTyVars:Coerce"
>
>	freeId id tvs = tyVarsOfType (idType id) `union` tvs
>	freeTy t  tvs = tyVarsOfType t `union` tvs
>	freeBind (v,e) tvs = freeId v (free e tvs)

> 	freeAtom (VarArg (DefArgExpr e)) tvs = free e tvs
> 	freeAtom (VarArg (Label l e)) tvs
> 		= panic "DefUtils(freeVars): VarArg (Label _ _)"
> 	freeAtom (VarArg (DefArgVar v)) tvs
> 		= panic "DefUtils(freeVars): VarArg (DefArgVar _ _)"
> 	freeAtom (LitArg l) tvs = tvs	-- XXX

> 	freeCaseAlts (AlgAlts as def) tvs
> 		= foldr freeAlgAlt  (freeDefault def tvs) as
> 	freeCaseAlts (PrimAlts as def) tvs
> 		= foldr freePrimAlt (freeDefault def tvs) as

> 	freeAlgAlt  (c, vs, e) tvs = foldr freeId (free e tvs) vs
> 	freePrimAlt (l, e) tvs = free e tvs

> 	freeDefault NoDefault tvs = tvs
> 	freeDefault (BindDefault v e) tvs = freeId v (free e tvs)

-----------------------------------------------------------------------------
\subsection{Rebinding variables in an expression}

Here is the code that renames all the bound variables in an expression
with new uniques.  Free variables are left unchanged.

> rebindExpr :: DefExpr -> UniqSM DefExpr
> rebindExpr e = uniqueExpr nullIdEnv nullTyVarEnv e

> uniqueExpr :: IdEnv Id -> TypeEnv -> DefExpr -> UniqSM DefExpr
> uniqueExpr p t e =
>   case e of
> 	Var (DefArgVar v) ->
> 		returnUs (Var (DefArgVar (lookup v p)))
>
> 	Var (Label l e) ->
> 		uniqueExpr p t l		`thenUs` \l ->
> 		uniqueExpr p t e		`thenUs` \e ->
> 		returnUs (mkLabel l e)
>
> 	Var (DefArgExpr _) ->
> 		panic "DefUtils(uniqueExpr): Var(DefArgExpr _)"
>
> 	Lit l ->
> 		returnUs e
>
> 	Con c ts es ->
> 		mapUs (uniqueAtom p t) es 	`thenUs` \es ->
> 		returnUs (Con c (map (applyTypeEnvToTy t) ts) es)
>
> 	Prim op ts es ->
> 		mapUs (uniqueAtom p t) es	 `thenUs` \es ->
> 		returnUs (Prim op (map (applyTypeEnvToTy t) ts) es)
>
> 	Lam vs e ->
> 		mapUs (newVar t) vs		`thenUs` \vs' ->
> 		uniqueExpr (growIdEnvList p (zip vs vs')) t e `thenUs` \e ->
> 		returnUs (Lam vs' e)
>
> 	CoTyLam v e ->
>		getUnique			`thenUs` \u ->
>		let v' = cloneTyVar v u
>		    t' = addOneToTyVarEnv t v (mkTyVarTy v') in
> 		uniqueExpr p t' e 		`thenUs` \e ->
> 		returnUs (CoTyLam v' e)
>
> 	App e v ->
> 		uniqueExpr p t e		`thenUs` \e ->
> 		uniqueAtom p t v		`thenUs` \v ->
> 		returnUs (App e v)
>
> 	CoTyApp e ty ->
> 		uniqueExpr p t e		`thenUs` \e ->
> 		returnUs (CoTyApp e (applyTypeEnvToTy t ty))
>
> 	Case e alts ->
> 		uniqueExpr p t e		`thenUs` \e ->
> 		uniqueAlts alts			`thenUs` \alts ->
> 		returnUs (Case e alts)
> 	     where
> 	     	uniqueAlts (AlgAlts  as d) =
> 			mapUs uniqueAlgAlt  as	`thenUs` \as ->
> 			uniqueDefault d		`thenUs` \d ->
> 			returnUs (AlgAlts as d)
> 		uniqueAlts (PrimAlts as d) =
> 			mapUs uniquePrimAlt as `thenUs` \as ->
> 			uniqueDefault d		`thenUs` \d ->
> 			returnUs (PrimAlts as d)
>
> 		uniqueAlgAlt (c, vs, e) =
> 			mapUs (newVar t) vs	`thenUs` \vs' ->
> 			uniqueExpr (growIdEnvList p (zip vs vs')) t e
>						`thenUs` \e ->
> 			returnUs (c, vs', e)
> 		uniquePrimAlt (l, e) =
> 			uniqueExpr p t e	`thenUs` \e ->
> 			returnUs (l, e)
>
> 		uniqueDefault NoDefault = returnUs NoDefault
> 		uniqueDefault (BindDefault v e) =
>			newVar t v	`thenUs` \v' ->
> 			uniqueExpr (addOneToIdEnv p v v') t e `thenUs` \e ->
> 			returnUs (BindDefault v' e)
>
> 	Let (NonRec v e) e' ->
> 		uniqueExpr p t e		`thenUs` \e ->
> 		newVar t v			`thenUs` \v' ->
> 		uniqueExpr (addOneToIdEnv p v v') t e'  `thenUs` \e' ->
> 		returnUs (Let (NonRec v' e) e')
>
> 	Let (Rec ds) e ->
> 		let (vs,es) = unzip ds in
> 		mapUs (newVar t) vs		`thenUs` \vs' ->
> 		let p' = growIdEnvList p (zip vs vs') in
> 		mapUs (uniqueExpr p' t) es  	`thenUs` \es ->
> 		uniqueExpr p' t e		`thenUs` \e ->
> 		returnUs (Let (Rec (zip vs' es)) e)
>
> 	SCC l e ->
> 		uniqueExpr p t e		`thenUs` \e ->
> 		returnUs (SCC l e)
>
>	Coerce _ _ _ -> panic "DefUtils.uniqueExpr:Coerce"
>
> uniqueAtom :: IdEnv Id -> TypeEnv -> DefAtom -> UniqSM DefAtom
> uniqueAtom p t (LitArg l) = returnUs (LitArg l) -- XXX
> uniqueAtom p t (VarArg v) =
> 	uniqueArg p t v	`thenUs` \v ->
>	returnUs (VarArg v)
>
> uniqueArg p t (DefArgVar v) =
> 	panic "DefUtils(uniqueArg): DefArgVar _ _"
> uniqueArg p t (DefArgExpr e) =
> 	uniqueExpr p t e	`thenUs` \e ->
> 	returnUs (DefArgExpr e)
> uniqueArg p t (Label l e) =
> 	panic "DefUtils(uniqueArg): Label _ _"

We shouldn't need to apply the type environment to free variables,
since their types can only contain type variables that are free in the
expression as a whole (?)

> lookup :: Id -> IdEnv Id -> Id
> lookup id p =
> 	case lookupIdEnv p id of
>		Nothing -> id
>		Just new_id -> new_id

> newVar :: TypeEnv -> Id -> UniqSM Id
> newVar t id =
> 	getUnique		`thenUs` \u ->
> 	returnUs (mkIdWithNewUniq (applyTypeEnvToId t id) u)

-----------------------------------------------------------------------------
\subsection{Detecting Renamings}

The function `renameExprs' takes two expressions and returns True if
they are renamings of each other.  The variables in the list `fs' are
excluded from the renaming process (i.e. if any of these variables
are present in one expression, they cannot be renamed in the other
expression).

We only allow renaming of sysLocal ids - ie. not top-level, imported
or otherwise global ids.

> data RenameResult
> 	= NotRenaming
>	| IsRenaming [(Id,Id)]
>	| InconsistentRenaming [(Id,Id)]

> renameExprs :: DefExpr -> DefExpr -> UniqSM RenameResult
> renameExprs u u' =
>	case ren u u' of
>		[]   -> returnUs NotRenaming
>		[r] -> if not (consistent r) then
>				d2c (strip u)	`thenUs` \u ->
>				d2c (strip u')  `thenUs` \u' ->
>				trace ("failed consistency check:\n" ++
>				       ppShow 80 (ppr PprDebug u) ++ "\n" ++
>				       ppShow 80 (ppr PprDebug u'))
>				(returnUs (InconsistentRenaming r))
>			else
>				trace "Renaming!" (returnUs (IsRenaming r))
>		_ -> panic "DefUtils(renameExprs)"

Check that we have a consistent renaming.  A renaming is consistent if
each time variable x in expression 1 is renamed, it is renamed to the
same variable.

> consistent :: [(Id,Id)] -> Bool
> consistent rs = and [ y == y' | (x,y) <- rs, (x',y') <- rs, x == x' ]

> checkConsistency :: [(Id,Id)] -> [[(Id,Id)]] -> [[(Id,Id)]]
> checkConsistency bound free = [ r' | r <- free, r' <- check r ]
> 	where
>	   check r | they're_consistent = [frees]
>		   | otherwise          = []
> 	   	where
>		   (bounds,frees) = partition (\(a,b) -> a `elem` lbound) r
>	           (lbound,rbound) = unzip bound
>	           they're_consistent = consistent (bound ++ bounds)

Renaming composition operator.

> (....) :: [[a]] -> [[a]] -> [[a]]
> r .... r' = [ xs ++ xs' | xs <- r, xs' <- r' ]

The class of identifiers which can be renamed.  It is sensible to
disallow renamings of deforestable ids, but the top-level ones are a
bit iffy.  Ideally, we should allow renaming of top-level ids, but the
current scheme allows us to leave out the top-level ids from the
argument lists of new function definitions.  (we still have the
shadowed ones to worry about..)

Main renaming function.  Returns a list of renamings made while
comparing the expressions.

> ren :: DefExpr -> DefExpr -> [[(Id,Id)]]
>
>	-- renaming or identical cases --
>
>
> 	-- same variable, no renaming
> ren (Var (DefArgVar x)) t@(Var (DefArgVar y))
> 	| x == y = [[(x,y)]]
>	| isArgId x && isArgId y = [[(x,y)]]
>
>	-- if we're doing matching, use the next rule,
>	-- and delete the second clause in the above rule.
> {-
> ren (Var (DefArgVar x)) t
> 	| okToRename x && all (not. deforestable) (freeVars t)
>	= [[(x,t)]]
> -}

> ren (Lit l) (Lit l') | l == l'
> 	= [[]]
> ren (Con c ts es) (Con c' ts' es') | c == c'
> 	= foldr (....) [[]] (zipWith renAtom es es')
> ren (Prim op ts es) (Prim op' ts' es') | op == op'
> 	= foldr (....) [[]] (zipWith renAtom es es')
> ren (Lam vs e) (Lam vs' e')
> 	= checkConsistency (zip vs vs') (ren e e')
> ren (CoTyLam vs e) (CoTyLam vs' e')
> 	= ren e e'			-- XXX!
> ren (App e v) (App e' v')
> 	= ren e e' .... renAtom v v'
> ren (CoTyApp e t) (CoTyApp e' t')
> 	= ren e e'			-- XXX!
> ren (Case e alts) (Case e' alts')
> 	= ren e e' .... renAlts alts alts'
> ren (Let (NonRec v a) b) (Let (NonRec v' a') b')
> 	= ren a a' .... (checkConsistency [(v,v')] (ren b b'))
> ren (Let (Rec ds) e) (Let (Rec ds') e')
> 	= checkConsistency (zip vs vs')
>		(ren e e' .... (foldr (....) [[]] (zipWith ren es es')))
>	where (vs ,es ) = unzip ds
>	      (vs',es') = unzip ds'
>
> 	-- label cases --
>
> ren (Var (Label l e)) e' 	= ren l e'
> ren e (Var (Label l e'))	= ren e l
>
>	-- error cases --
>
> ren (Var (DefArgExpr _)) _
> 	= panic "DefUtils(ren): Var (DefArgExpr _)"
> ren _ (Var (DefArgExpr _))
> 	= panic "DefUtils(ren): Var (DefArgExpr _)"
>
>	-- default case --
>
> ren _ _ = []

Rename atoms.

> renAtom (VarArg (DefArgExpr e)) (VarArg (DefArgExpr e'))
> 	= ren e e'
>  -- XXX shouldn't need the next two
> renAtom (LitArg l) (LitArg l') | l == l' = [[]]
> renAtom (VarArg (DefArgVar v)) _ =
> 	panic "DefUtils(renAtom): VarArg (DefArgVar _ _)"
> renAtom _ (VarArg (DefArgVar v)) =
> 	panic "DefUtils(renAtom): VarArg (DefArgVar _ _)"
> renAtom (VarArg (Label _ _)) _ =
> 	panic "DefUtils(renAtom): VarArg (Label _ _)"
> renAtom e (VarArg (Label l e')) =
> 	panic "DefUtils(renAtom): VarArg (Label _ _)"
>
> renAtom _ _ = []

Renamings of case alternatives doesn't allow reordering, but that
should be Ok (we don't ever change the ordering anyway).

> renAlts (AlgAlts as dflt) (AlgAlts as' dflt')
> 	= foldr (....) [[]] (zipWith renAlgAlt as as') .... renDefault dflt dflt'
> renAlts (PrimAlts as dflt) (PrimAlts as' dflt')
> 	= foldr (....) [[]] (zipWith renPrimAlt as as') .... renDefault dflt dflt'
> renAlts _ _ = []
>
> renAlgAlt (c,vs,e) (c',vs',e') | c == c'
> 	= checkConsistency (zip vs vs') (ren e e')
> renAlgAlt _ _ = []
>
> renPrimAlt (l,e) (l',e') | l == l' = ren e e'
> renPrimAlt _ _ = []
>
> renDefault NoDefault NoDefault = [[]]
> renDefault (BindDefault v e) (BindDefault v' e')
> 	= checkConsistency [(v,v')] (ren e e')

-----------------------------------------------------------------------------

> atom2expr :: DefAtom -> DefExpr
> atom2expr (VarArg (DefArgExpr e)) = e
> atom2expr (VarArg (Label l e)) = mkLabel l e
> -- XXX next two should be illegal
> atom2expr (LitArg l) = Lit l
> atom2expr (VarArg (DefArgVar v)) =
> 	panic "DefUtils(atom2expr): VarArg (DefArgVar _)"

> expr2atom = VarArg . DefArgExpr

-----------------------------------------------------------------------------
Grab a new Id and tag it as coming from the Deforester.

> newDefId :: Type -> UniqSM Id
> newDefId t =
> 	getUnique	`thenUs` \u ->
>	returnUs (mkSysLocal SLIT("def") u t mkUnknownSrcLoc)

> newTmpId :: Type -> UniqSM Id
> newTmpId t =
> 	getUnique	`thenUs` \u ->
>	returnUs (mkSysLocal SLIT("tmp") u t mkUnknownSrcLoc)

-----------------------------------------------------------------------------
Check whether an Id was given a `DEFOREST' annotation by the programmer.

> deforestable :: Id -> Bool
> deforestable id =
> 	case getInfo (getIdInfo id) of
>		DoDeforest -> True
>		Don'tDeforest -> False

-----------------------------------------------------------------------------
Filter for free variables to abstract from new functions.

> isArgId id
> 	=    (not . deforestable)  id
>         && (not . toplevelishId) id

-----------------------------------------------------------------------------

> foldrSUs f c [] = returnUs c
> foldrSUs f c (x:xs)
> 	= foldrSUs f c xs	`thenUs` \xs' ->
>	  f x xs'

-----------------------------------------------------------------------------

> mkDefLetrec [] e = e
> mkDefLetrec bs e = Let (Rec bs) e

-----------------------------------------------------------------------------
Substitutions.

> subst :: [(Id,DefExpr)]
> 	-> DefExpr
>	-> UniqSM DefExpr

> subst p e' = sub e'
>  where
>     p' = mkIdEnv p
>     sub e' = case e' of
> 	Var (DefArgExpr e) -> panic "DefExpr(sub): Var (DefArgExpr _)"
>	Var (Label l e)    -> panic "DefExpr(sub): Var (Label _ _)"
>       Var (DefArgVar v) ->
>		case lookupIdEnv p' v of
>			Just e  -> rebindExpr e	`thenUs` \e -> returnUs e
>			Nothing -> returnUs e'
>       Lit l              -> returnUs e'
>       Con c ts es        -> mapUs substAtom es	`thenUs` \es ->
>				returnUs (Con c ts es)
>       Prim op ts es      -> mapUs substAtom es	`thenUs` \es ->
>				returnUs (Prim op ts es)
>       Lam vs e           -> sub e			`thenUs` \e ->
>				returnUs (Lam vs e)
>       CoTyLam alpha e      -> sub e			`thenUs` \e ->
>				returnUs (CoTyLam alpha e)
>       App e v            -> sub e			`thenUs` \e ->
>				substAtom v		`thenUs` \v ->
>				returnUs (App e v)
>       CoTyApp e t          -> sub e			`thenUs` \e ->
>				returnUs (CoTyApp e t)
>       Case e ps          -> sub e			`thenUs` \e ->
>				substCaseAlts ps	`thenUs` \ps ->
>				returnUs (Case e ps)
>       Let (NonRec v e) e'
>			     -> sub e			`thenUs` \e ->
>			        sub e'			`thenUs` \e' ->
>				returnUs (Let (NonRec v e) e')
>       Let (Rec bs) e   -> sub e			`thenUs` \e ->
>				mapUs substBind bs	`thenUs` \bs ->
>				returnUs (Let (Rec bs) e)
>			where
>				substBind (v,e) =
>					sub e 		`thenUs` \e ->
>					returnUs (v,e)
>       SCC l e            -> sub e			`thenUs` \e ->
>				returnUs (SCC l e)
>
>	Coerce _ _ _ -> panic "DefUtils.subst:Coerce"

>     substAtom (VarArg v) =
>     		substArg v `thenUs` \v ->
>		returnUs (VarArg v)
>     substAtom (LitArg l) =
>     		returnUs (LitArg l)	-- XXX

>     substArg (DefArgExpr e) =
>     		sub e		`thenUs` \e ->
>		returnUs (DefArgExpr e)
>     substArg e@(Label _ _)  =
>     		panic "DefExpr(substArg): Label _ _"
>     substArg e@(DefArgVar v)  =	-- XXX
>     		case lookupIdEnv p' v of
>			Just e -> rebindExpr e	`thenUs` \e ->
>				  returnUs (DefArgExpr e)
>			Nothing -> returnUs e

>     substCaseAlts (AlgAlts as def) =
>     		mapUs substAlgAlt as		`thenUs` \as ->
>		substDefault def		`thenUs` \def ->
>		returnUs (AlgAlts as def)
>     substCaseAlts (PrimAlts as def) =
>     		mapUs substPrimAlt as		`thenUs` \as ->
>		substDefault def		`thenUs` \def ->
>		returnUs (PrimAlts as def)

>     substAlgAlt  (c, vs, e) =
>     		sub e				`thenUs` \e ->
>		returnUs (c, vs, e)
>     substPrimAlt (l, e) =
>     		sub e				`thenUs` \e ->
>		returnUs (l, e)

>     substDefault NoDefault =
>     		returnUs NoDefault
>     substDefault (BindDefault v e) =
>     		sub e				`thenUs` \e ->
>		returnUs (BindDefault v e)

-----------------------------------------------------------------------------

> union [] ys = ys
> union (x:xs) ys
> 	| x `is_elem` ys = union xs ys
>	| otherwise   = x : union xs ys
>   where { is_elem = isIn "union(deforest)" }

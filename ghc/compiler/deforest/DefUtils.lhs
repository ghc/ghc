%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
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

> import AbsUniType	( cloneTyVar, mkTyVarTy, applyTypeEnvToTy, 
> 			  extractTyVarsFromTy, TyVar, SigmaType(..)
>			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
>			)
> import BasicLit	( BasicLit )	-- for Eq BasicLit
> import CoreSyn
> import Id		( mkIdWithNewUniq, mkSysLocal, applyTypeEnvToId,
> 			  getIdInfo, toplevelishId, getIdUniType, Id )
> import IdEnv
> import IdInfo
> import Outputable
> import Pretty
> import PrimOps	( PrimOp )	-- for Eq PrimOp
> import SplitUniq
> import SrcLoc		( mkUnknownSrcLoc )
> import TyVarEnv
> import Util

-----------------------------------------------------------------------------
\susbsection{Strip}

Implementation of the strip function.  Strip is the identity on
expressions (recursing into subterms), but replaces each label with
its left hand side.  The result is a term with no labels.

> strip :: DefExpr -> DefExpr

> strip e' = case e' of
> 	CoVar (DefArgExpr e) -> panic "DefUtils(strip): CoVar (DefExpr _)"
>	CoVar (Label l e)    -> l
>       CoVar (DefArgVar v)  -> e'
>       CoLit l              -> e'
>       CoCon c ts es        -> CoCon c ts (map stripAtom es)
>       CoPrim op ts es      -> CoPrim op ts (map stripAtom es)
>       CoLam vs e           -> CoLam vs (strip e)
>       CoTyLam alpha e      -> CoTyLam alpha (strip e)
>       CoApp e v            -> CoApp (strip e) (stripAtom v)
>       CoTyApp e t          -> CoTyApp (strip e) t
>       CoCase e ps          -> CoCase (strip e) (stripCaseAlts ps)
>       CoLet (CoNonRec v e) e' -> CoLet (CoNonRec v (strip e)) (strip e')
>       CoLet (CoRec bs) e   -> 
>		CoLet (CoRec [ (v, strip e) | (v,e) <- bs ]) (strip e)
>       CoSCC l e            -> CoSCC l (strip e)

> stripAtom :: DefAtom -> DefAtom
> stripAtom (CoVarAtom v) = CoVarAtom (stripArg v)
> stripAtom (CoLitAtom l) = CoLitAtom l	-- XXX

> stripArg :: DefBindee -> DefBindee
> stripArg (DefArgExpr e) = DefArgExpr (strip e)
> stripArg (Label l e)   = panic "DefUtils(stripArg): Label _ _"
> stripArg (DefArgVar v) = panic "DefUtils(stripArg): DefArgVar _ _"

> stripCaseAlts (CoAlgAlts as def) 
> 	= CoAlgAlts (map stripAlgAlt as) (stripDefault def)
> stripCaseAlts (CoPrimAlts as def) 
> 	= CoPrimAlts (map stripPrimAlt as) (stripDefault def)

> stripAlgAlt  (c, vs, e) = (c, vs, strip e)
> stripPrimAlt (l, e) = (l, strip e)

> stripDefault CoNoDefault = CoNoDefault
> stripDefault (CoBindDefault v e) = CoBindDefault v (strip e)

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
>		CoVar (DefArgExpr e) -> 
>			panic "DefUtils(free): CoVar (DefExpr _)"
>		CoVar (Label l e)    -> free l fvs
>       	CoVar (DefArgVar v)
>			| v `is_elem` fvs	-> fvs
>			| otherwise	-> v : fvs
>		  where { is_elem = isIn "freeVars(deforest)" }
>       	CoLit l              -> fvs
>       	CoCon c ts es        -> foldr freeAtom fvs es
>       	CoPrim op ts es      -> foldr freeAtom fvs es
>       	CoLam vs e           -> free' vs (free e fvs)
>       	CoTyLam alpha e      -> free e fvs
>       	CoApp 	e v          -> free e (freeAtom v fvs)
>       	CoTyApp e t          -> free e fvs
>       	CoCase e ps          -> free e (freeCaseAlts ps fvs)
>       	CoLet (CoNonRec v e) e' -> free e (free' [v] (free e' fvs))
>       	CoLet (CoRec bs) e   -> free' vs (foldr free (free e fvs) es)
>			where (vs,es) = unzip bs
>       	CoSCC l e            -> free e fvs

>	free' :: [Id] -> [Id] -> [Id]
> 	free' vs fvs = filter (\x -> notElem x vs) fvs

> 	freeAtom (CoVarAtom (DefArgExpr e)) fvs = free e fvs
> 	freeAtom (CoVarAtom (Label l e)) fvs 
> 		= panic "DefUtils(free): CoVarAtom (Label _ _)"
> 	freeAtom (CoVarAtom (DefArgVar v)) fvs
> 		= panic "DefUtils(free): CoVarAtom (DefArgVar _ _)"
> 	freeAtom (CoLitAtom l) fvs = fvs

> 	freeCaseAlts (CoAlgAlts as def) fvs
> 		= foldr freeAlgAlt  (freeDefault def fvs) as
> 	freeCaseAlts (CoPrimAlts as def) fvs
> 		= foldr freePrimAlt (freeDefault def fvs) as
>		
> 	freeAlgAlt  (c, vs, e) fvs = free' vs (free e fvs)
> 	freePrimAlt (l, e) fvs = free e fvs

> 	freeDefault CoNoDefault fvs = fvs
> 	freeDefault (CoBindDefault v e) fvs = free' [v] (free e fvs)

-----------------------------------------------------------------------------
\subsection{Free Type Variables}

> freeTyVars :: DefExpr -> [TyVar]
> freeTyVars e = free e []
>   where
>   	free e tvs = case e of
>		CoVar (DefArgExpr e)    ->
>			panic "DefUtils(freeVars): CoVar (DefExpr _)"
>		CoVar (Label l e)       -> free l tvs
>       	CoVar (DefArgVar id)    -> freeId id tvs
>       	CoLit l                 -> tvs
>       	CoCon c ts es           -> foldr freeTy (foldr freeAtom tvs es) ts
>       	CoPrim op ts es         -> foldr freeTy (foldr freeAtom tvs es) ts
>       	CoLam vs e              -> foldr freeId (free e tvs) vs
>       	CoTyLam alpha e         -> filter (/= alpha) (free e tvs)
>       	CoApp e v               -> free e (freeAtom v tvs)
>       	CoTyApp e t             -> free e (freeTy t tvs)
>       	CoCase e ps             -> free e (freeCaseAlts ps tvs)
>       	CoLet (CoNonRec v e) e' -> free e (freeId v (free e' tvs))
>       	CoLet (CoRec bs) e      -> foldr freeBind (free e tvs) bs
>       	CoSCC l e               -> free e tvs
>		
>	freeId id tvs = extractTyVarsFromTy (getIdUniType id) `union` tvs
>	freeTy t  tvs = extractTyVarsFromTy t `union` tvs
>	freeBind (v,e) tvs = freeId v (free e tvs)
  
> 	freeAtom (CoVarAtom (DefArgExpr e)) tvs = free e tvs
> 	freeAtom (CoVarAtom (Label l e)) tvs
> 		= panic "DefUtils(freeVars): CoVarAtom (Label _ _)"
> 	freeAtom (CoVarAtom (DefArgVar v)) tvs
> 		= panic "DefUtils(freeVars): CoVarAtom (DefArgVar _ _)"
> 	freeAtom (CoLitAtom l) tvs = tvs	-- XXX

> 	freeCaseAlts (CoAlgAlts as def) tvs
> 		= foldr freeAlgAlt  (freeDefault def tvs) as
> 	freeCaseAlts (CoPrimAlts as def) tvs
> 		= foldr freePrimAlt (freeDefault def tvs) as

> 	freeAlgAlt  (c, vs, e) tvs = foldr freeId (free e tvs) vs
> 	freePrimAlt (l, e) tvs = free e tvs

> 	freeDefault CoNoDefault tvs = tvs
> 	freeDefault (CoBindDefault v e) tvs = freeId v (free e tvs)

-----------------------------------------------------------------------------
\subsection{Rebinding variables in an expression}

Here is the code that renames all the bound variables in an expression
with new uniques.  Free variables are left unchanged.

> rebindExpr :: DefExpr -> SUniqSM DefExpr
> rebindExpr e = uniqueExpr nullIdEnv nullTyVarEnv e

> uniqueExpr :: IdEnv Id -> TypeEnv -> DefExpr -> SUniqSM DefExpr
> uniqueExpr p t e =
>   case e of
> 	CoVar (DefArgVar v) -> 
> 		returnSUs (CoVar (DefArgVar (lookup v p)))
> 	
> 	CoVar (Label l e) -> 
> 		uniqueExpr p t l		`thenSUs` \l ->
> 		uniqueExpr p t e		`thenSUs` \e ->
> 		returnSUs (mkLabel l e)
> 		
> 	CoVar (DefArgExpr _) ->
> 		panic "DefUtils(uniqueExpr): CoVar(DefArgExpr _)"
> 		
> 	CoLit l ->
> 		returnSUs e
> 		
> 	CoCon c ts es ->
> 		mapSUs (uniqueAtom p t) es 	`thenSUs` \es ->
> 		returnSUs (CoCon c (map (applyTypeEnvToTy t) ts) es)
> 		
> 	CoPrim op ts es ->
> 		mapSUs (uniqueAtom p t) es	 `thenSUs` \es ->
> 		returnSUs (CoPrim op (map (applyTypeEnvToTy t) ts) es)
> 		
> 	CoLam vs e ->
> 		mapSUs (newVar t) vs		`thenSUs` \vs' ->
> 		uniqueExpr (growIdEnvList p (zip vs vs')) t e `thenSUs` \e ->
> 		returnSUs (CoLam vs' e)
> 		
> 	CoTyLam v e ->
>		getSUnique			`thenSUs` \u ->
>		let v' = cloneTyVar v u
>		    t' = addOneToTyVarEnv t v (mkTyVarTy v') in
> 		uniqueExpr p t' e 		`thenSUs` \e ->
> 		returnSUs (CoTyLam v' e)
> 	
> 	CoApp e v ->
> 		uniqueExpr p t e		`thenSUs` \e ->
> 		uniqueAtom p t v		`thenSUs` \v ->
> 		returnSUs (CoApp e v)
> 		
> 	CoTyApp e ty ->
> 		uniqueExpr p t e		`thenSUs` \e ->
> 		returnSUs (mkCoTyApp e (applyTypeEnvToTy t ty))
> 	
> 	CoCase e alts ->
> 		uniqueExpr p t e		`thenSUs` \e ->
> 		uniqueAlts alts			`thenSUs` \alts ->
> 		returnSUs (CoCase e alts)
> 	     where
> 	     	uniqueAlts (CoAlgAlts  as d) = 
> 			mapSUs uniqueAlgAlt  as	`thenSUs` \as ->
> 			uniqueDefault d		`thenSUs` \d ->
> 			returnSUs (CoAlgAlts as d)
> 		uniqueAlts (CoPrimAlts as d) =
> 			mapSUs uniquePrimAlt as `thenSUs` \as ->
> 			uniqueDefault d		`thenSUs` \d ->
> 			returnSUs (CoPrimAlts as d)
> 			
> 		uniqueAlgAlt (c, vs, e) = 
> 			mapSUs (newVar t) vs	`thenSUs` \vs' ->
> 			uniqueExpr (growIdEnvList p (zip vs vs')) t e 
>						`thenSUs` \e ->
> 			returnSUs (c, vs', e)
> 		uniquePrimAlt (l, e) =
> 			uniqueExpr p t e	`thenSUs` \e ->
> 			returnSUs (l, e)
> 			
> 		uniqueDefault CoNoDefault = returnSUs CoNoDefault
> 		uniqueDefault (CoBindDefault v e) = 
>			newVar t v	`thenSUs` \v' ->
> 			uniqueExpr (addOneToIdEnv p v v') t e `thenSUs` \e ->
> 			returnSUs (CoBindDefault v' e)
> 
> 	CoLet (CoNonRec v e) e' ->
> 		uniqueExpr p t e		`thenSUs` \e ->
> 		newVar t v			`thenSUs` \v' ->
> 		uniqueExpr (addOneToIdEnv p v v') t e'  `thenSUs` \e' ->
> 		returnSUs (CoLet (CoNonRec v' e) e')
> 		
> 	CoLet (CoRec ds) e ->
> 		let (vs,es) = unzip ds in
> 		mapSUs (newVar t) vs		`thenSUs` \vs' ->
> 		let p' = growIdEnvList p (zip vs vs') in
> 		mapSUs (uniqueExpr p' t) es  	`thenSUs` \es ->
> 		uniqueExpr p' t e		`thenSUs` \e ->
> 		returnSUs (CoLet (CoRec (zip vs' es)) e)
> 
> 	CoSCC l e ->
> 		uniqueExpr p t e		`thenSUs` \e ->
> 		returnSUs (CoSCC l e)
> 		
> 
> uniqueAtom :: IdEnv Id -> TypeEnv -> DefAtom -> SUniqSM DefAtom
> uniqueAtom p t (CoLitAtom l) = returnSUs (CoLitAtom l) -- XXX
> uniqueAtom p t (CoVarAtom v) = 
> 	uniqueArg p t v	`thenSUs` \v ->
>	returnSUs (CoVarAtom v)
> 
> uniqueArg p t (DefArgVar v) =
> 	panic "DefUtils(uniqueArg): DefArgVar _ _"
> uniqueArg p t (DefArgExpr e) =
> 	uniqueExpr p t e	`thenSUs` \e ->
> 	returnSUs (DefArgExpr e)
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

> newVar :: TypeEnv -> Id -> SUniqSM Id
> newVar t id = 
> 	getSUnique		`thenSUs` \u ->
> 	returnSUs (mkIdWithNewUniq (applyTypeEnvToId t id) u)

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

> renameExprs :: DefExpr -> DefExpr -> SUniqSM RenameResult
> renameExprs u u' = 
>	case ren u u' of
>		[]   -> returnSUs NotRenaming
>		[r] -> if not (consistent r) then 
>				d2c (strip u)	`thenSUs` \u ->
>				d2c (strip u')  `thenSUs` \u' ->
>				trace ("failed consistency check:\n" ++
>				       ppShow 80 (ppr PprDebug u) ++ "\n" ++
>				       ppShow 80 (ppr PprDebug u'))
>				(returnSUs (InconsistentRenaming r))
>			else 
>				trace "Renaming!" (returnSUs (IsRenaming r))
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
> ren (CoVar (DefArgVar x)) t@(CoVar (DefArgVar y)) 
> 	| x == y = [[(x,y)]]
>	| isArgId x && isArgId y = [[(x,y)]]
>
>	-- if we're doing matching, use the next rule,
>	-- and delete the second clause in the above rule.
> {-
> ren (CoVar (DefArgVar x)) t 
> 	| okToRename x && all (not. deforestable) (freeVars t)
>	= [[(x,t)]]
> -}

> ren (CoLit l) (CoLit l') | l == l'
> 	= [[]]
> ren (CoCon c ts es) (CoCon c' ts' es') | c == c'
> 	= foldr (....) [[]] (zipWith renAtom es es')
> ren (CoPrim op ts es) (CoPrim op' ts' es') | op == op'
> 	= foldr (....) [[]] (zipWith renAtom es es')
> ren (CoLam vs e) (CoLam vs' e')
> 	= checkConsistency (zip vs vs') (ren e e')
> ren (CoTyLam vs e) (CoTyLam vs' e')
> 	= ren e e'			-- XXX!
> ren (CoApp e v) (CoApp e' v')
> 	= ren e e' .... renAtom v v'
> ren (CoTyApp e t) (CoTyApp e' t')
> 	= ren e e'			-- XXX!
> ren (CoCase e alts) (CoCase e' alts')
> 	= ren e e' .... renAlts alts alts'
> ren (CoLet (CoNonRec v a) b) (CoLet (CoNonRec v' a') b')
> 	= ren a a' .... (checkConsistency [(v,v')] (ren b b'))
> ren (CoLet (CoRec ds) e) (CoLet (CoRec ds') e')
> 	= checkConsistency (zip vs vs') 
>		(ren e e' .... (foldr (....) [[]] (zipWith ren es es')))
>	where (vs ,es ) = unzip ds
>	      (vs',es') = unzip ds'
>	   
> 	-- label cases --
>	
> ren (CoVar (Label l e)) e' 	= ren l e'
> ren e (CoVar (Label l e'))	= ren e l
>
>	-- error cases --
>	
> ren (CoVar (DefArgExpr _)) _
> 	= panic "DefUtils(ren): CoVar (DefArgExpr _)"
> ren _ (CoVar (DefArgExpr _))
> 	= panic "DefUtils(ren): CoVar (DefArgExpr _)"
>	
>	-- default case --
>	
> ren _ _ = [] 

Rename atoms.

> renAtom (CoVarAtom (DefArgExpr e)) (CoVarAtom (DefArgExpr e'))
> 	= ren e e'
>  -- XXX shouldn't need the next two
> renAtom (CoLitAtom l) (CoLitAtom l') | l == l' = [[]]				
> renAtom (CoVarAtom (DefArgVar v)) _ =
> 	panic "DefUtils(renAtom): CoVarAtom (DefArgVar _ _)"
> renAtom _ (CoVarAtom (DefArgVar v)) =
> 	panic "DefUtils(renAtom): CoVarAtom (DefArgVar _ _)"
> renAtom (CoVarAtom (Label _ _)) _ = 
> 	panic "DefUtils(renAtom): CoVarAtom (Label _ _)"
> renAtom e (CoVarAtom (Label l e')) =
> 	panic "DefUtils(renAtom): CoVarAtom (Label _ _)"
>	
> renAtom _ _ = []

Renamings of case alternatives doesn't allow reordering, but that
should be Ok (we don't ever change the ordering anyway).

> renAlts (CoAlgAlts as dflt) (CoAlgAlts as' dflt')
> 	= foldr (....) [[]] (zipWith renAlgAlt as as') .... renDefault dflt dflt'
> renAlts (CoPrimAlts as dflt) (CoPrimAlts as' dflt')
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
> renDefault CoNoDefault CoNoDefault = [[]]
> renDefault (CoBindDefault v e) (CoBindDefault v' e')
> 	= checkConsistency [(v,v')] (ren e e')

-----------------------------------------------------------------------------

> atom2expr :: DefAtom -> DefExpr
> atom2expr (CoVarAtom (DefArgExpr e)) = e
> atom2expr (CoVarAtom (Label l e)) = mkLabel l e
> -- XXX next two should be illegal
> atom2expr (CoLitAtom l) = CoLit l
> atom2expr (CoVarAtom (DefArgVar v)) = 
> 	panic "DefUtils(atom2expr): CoVarAtom (DefArgVar _)"

> expr2atom = CoVarAtom . DefArgExpr

-----------------------------------------------------------------------------
Grab a new Id and tag it as coming from the Deforester.

> newDefId :: UniType -> SUniqSM Id
> newDefId t = 
> 	getSUnique	`thenSUs` \u ->
>	returnSUs (mkSysLocal SLIT("def") u t mkUnknownSrcLoc)

> newTmpId :: UniType -> SUniqSM Id
> newTmpId t =
> 	getSUnique	`thenSUs` \u ->
>	returnSUs (mkSysLocal SLIT("tmp") u t mkUnknownSrcLoc)

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

> foldrSUs f c [] = returnSUs c
> foldrSUs f c (x:xs)
> 	= foldrSUs f c xs	`thenSUs` \xs' ->
>	  f x xs'

-----------------------------------------------------------------------------

> mkDefLetrec [] e = e
> mkDefLetrec bs e = CoLet (CoRec bs) e

-----------------------------------------------------------------------------
Substitutions.

> subst :: [(Id,DefExpr)]
> 	-> DefExpr
>	-> SUniqSM DefExpr

> subst p e' = sub e'
>  where
>     p' = mkIdEnv p
>     sub e' = case e' of
> 	CoVar (DefArgExpr e) -> panic "DefExpr(sub): CoVar (DefArgExpr _)"
>	CoVar (Label l e)    -> panic "DefExpr(sub): CoVar (Label _ _)"
>       CoVar (DefArgVar v) ->
>		case lookupIdEnv p' v of
>			Just e  -> rebindExpr e	`thenSUs` \e -> returnSUs e
>			Nothing -> returnSUs e'
>       CoLit l              -> returnSUs e'
>       CoCon c ts es        -> mapSUs substAtom es	`thenSUs` \es ->
>				returnSUs (CoCon c ts es)
>       CoPrim op ts es      -> mapSUs substAtom es	`thenSUs` \es ->
>				returnSUs (CoPrim op ts es)
>       CoLam vs e           -> sub e			`thenSUs` \e ->
>				returnSUs (CoLam vs e)
>       CoTyLam alpha e      -> sub e			`thenSUs` \e ->
>				returnSUs (CoTyLam alpha e)
>       CoApp e v            -> sub e			`thenSUs` \e ->
>				substAtom v		`thenSUs` \v ->
>				returnSUs (CoApp e v)
>       CoTyApp e t          -> sub e			`thenSUs` \e ->
>				returnSUs (CoTyApp e t)
>       CoCase e ps          -> sub e			`thenSUs` \e ->
>				substCaseAlts ps	`thenSUs` \ps ->
>				returnSUs (CoCase e ps)
>       CoLet (CoNonRec v e) e' 
>			     -> sub e			`thenSUs` \e ->
>			        sub e'			`thenSUs` \e' ->
>				returnSUs (CoLet (CoNonRec v e) e')
>       CoLet (CoRec bs) e   -> sub e			`thenSUs` \e ->
>				mapSUs substBind bs	`thenSUs` \bs ->
>				returnSUs (CoLet (CoRec bs) e)
>			where
>				substBind (v,e) = 
>					sub e 		`thenSUs` \e ->
>					returnSUs (v,e)
>       CoSCC l e            -> sub e			`thenSUs` \e ->
>				returnSUs (CoSCC l e)

>     substAtom (CoVarAtom v) = 
>     		substArg v `thenSUs` \v ->
>		returnSUs (CoVarAtom v)
>     substAtom (CoLitAtom l) = 
>     		returnSUs (CoLitAtom l)	-- XXX

>     substArg (DefArgExpr e) = 
>     		sub e		`thenSUs` \e ->
>		returnSUs (DefArgExpr e)
>     substArg e@(Label _ _)  = 
>     		panic "DefExpr(substArg): Label _ _"
>     substArg e@(DefArgVar v)  =	-- XXX
>     		case lookupIdEnv p' v of
>			Just e -> rebindExpr e	`thenSUs` \e ->
>				  returnSUs (DefArgExpr e)
>			Nothing -> returnSUs e

>     substCaseAlts (CoAlgAlts as def) = 
>     		mapSUs substAlgAlt as		`thenSUs` \as ->
>		substDefault def		`thenSUs` \def ->
>		returnSUs (CoAlgAlts as def)
>     substCaseAlts (CoPrimAlts as def) =
>     		mapSUs substPrimAlt as		`thenSUs` \as ->
>		substDefault def		`thenSUs` \def ->
>		returnSUs (CoPrimAlts as def)

>     substAlgAlt  (c, vs, e) = 
>     		sub e				`thenSUs` \e ->
>		returnSUs (c, vs, e)
>     substPrimAlt (l, e) = 
>     		sub e				`thenSUs` \e ->
>		returnSUs (l, e)

>     substDefault CoNoDefault = 
>     		returnSUs CoNoDefault
>     substDefault (CoBindDefault v e) = 
>     		sub e				`thenSUs` \e ->
>		returnSUs (CoBindDefault v e)

-----------------------------------------------------------------------------

> union [] ys = ys
> union (x:xs) ys 
> 	| x `is_elem` ys = union xs ys
>	| otherwise   = x : union xs ys
>   where { is_elem = isIn "union(deforest)" }

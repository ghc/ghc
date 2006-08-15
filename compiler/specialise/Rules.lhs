%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreRules]{Transformation rules}

\begin{code}
module Rules (
	RuleBase, emptyRuleBase, mkRuleBase, extendRuleBaseList, 
	unionRuleBase, pprRuleBase, ruleCheckProgram,

	mkSpecInfo, extendSpecInfo, addSpecInfo,
	rulesOfBinds, addIdSpecialisations, 
	
	matchN,

        lookupRule, mkLocalRule, roughTopNames
    ) where

#include "HsVersions.h"

import CoreSyn		-- All of it
import OccurAnal	( occurAnalyseExpr )
import CoreFVs		( exprFreeVars, exprsFreeVars, bindFreeVars, rulesRhsFreeVars )
import CoreUnfold	( isCheapUnfolding, unfoldingTemplate )
import CoreUtils	( tcEqExprX )
import PprCore		( pprRules )
import Type		( TvSubstEnv )
import TcType		( tcSplitTyConApp_maybe )
import CoreTidy		( tidyRules )
import Id		( Id, idUnfolding, isLocalId, isGlobalId, idName,
			  idSpecialisation, idCoreRules, setIdSpecialisation ) 
import IdInfo		( SpecInfo( SpecInfo ) )
import Var		( Var )
import VarEnv		( IdEnv, InScopeSet, emptyTidyEnv,
			  emptyInScopeSet, mkInScopeSet, extendInScopeSetList, 
			  emptyVarEnv, lookupVarEnv, extendVarEnv, 
			  nukeRnEnvL, mkRnEnv2, rnOccR, rnOccL, inRnEnvR,
			  rnBndrR, rnBndr2, rnBndrL, rnBndrs2,
			  rnInScope, extendRnInScopeList, lookupRnInScope )
import VarSet
import Name		( Name, NamedThing(..), nameOccName )
import NameEnv
import Unify 		( ruleMatchTyX, MatchEnv(..) )
import BasicTypes	( Activation, CompilerPhase, isActive )
import Outputable
import FastString
import Maybes		( isJust, orElse )
import OrdList
import Bag
import Util		( singleton, mapAccumL )
import List		( isPrefixOf )
\end{code}


%************************************************************************
%*									*
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
%*									*
%************************************************************************

A @CoreRule@ holds details of one rule for an @Id@, which
includes its specialisations.

For example, if a rule for @f@ contains the mapping:
\begin{verbatim}
	forall a b d. [Type (List a), Type b, Var d]  ===>  f' a b
\end{verbatim}
then when we find an application of f to matching types, we simply replace
it by the matching RHS:
\begin{verbatim}
	f (List Int) Bool dict ===>  f' Int Bool
\end{verbatim}
All the stuff about how many dictionaries to discard, and what types
to apply the specialised function to, are handled by the fact that the
Rule contains a template for the result of the specialisation.

There is one more exciting case, which is dealt with in exactly the same
way.  If the specialised value is unboxed then it is lifted at its
definition site and unlifted at its uses.  For example:

	pi :: forall a. Num a => a

might have a specialisation

	[Int#] ===>  (case pi' of Lift pi# -> pi#)

where pi' :: Lift Int# is the specialised version of pi.

\begin{code}
mkLocalRule :: RuleName -> Activation 
	    -> Name -> [CoreBndr] -> [CoreExpr] -> CoreExpr -> CoreRule
-- Used to make CoreRule for an Id defined in this module
mkLocalRule name act fn bndrs args rhs
  = Rule { ru_name = name, ru_fn = fn, ru_act = act,
	   ru_bndrs = bndrs, ru_args = args,
	   ru_rhs = rhs, ru_rough = roughTopNames args,
	   ru_orph = Just (nameOccName fn), ru_local = True }

--------------
roughTopNames :: [CoreExpr] -> [Maybe Name]
roughTopNames args = map roughTopName args

roughTopName :: CoreExpr -> Maybe Name
-- Find the "top" free name of an expression
-- a) the function in an App chain (if a GlobalId)
-- b) the TyCon in a type
-- This is used for the fast-match-check for rules; 
--	if the top names don't match, the rest can't
roughTopName (Type ty) = case tcSplitTyConApp_maybe ty of
			  Just (tc,_) -> Just (getName tc)
			  Nothing     -> Nothing
roughTopName (App f a) = roughTopName f
roughTopName (Var f) | isGlobalId f = Just (idName f)
		     | otherwise    = Nothing
roughTopName other = Nothing

ruleCantMatch :: [Maybe Name] -> [Maybe Name] -> Bool
-- (ruleCantMatch tpl actual) returns True only if 'actual'
-- definitely can't match 'tpl' by instantiating 'tpl'.  
-- It's only a one-way match; unlike instance matching we 
-- don't consider unification
-- 
-- Notice that there is no case
--	ruleCantMatch (Just n1 : ts) (Nothing : as) = True
-- Reason: a local variable 'v' in the actuals might 
-- 	   have an unfolding which is a global.
--	   This quite often happens with case scrutinees.
ruleCantMatch (Just n1 : ts) (Just n2 : as) = n1 /= n2 || ruleCantMatch ts as
ruleCantMatch (t       : ts) (a       : as) = ruleCantMatch ts as
ruleCantMatch ts	     as		    = False
\end{code}


%************************************************************************
%*									*
		SpecInfo: the rules in an IdInfo
%*									*
%************************************************************************

\begin{code}
mkSpecInfo :: [CoreRule] -> SpecInfo
mkSpecInfo rules = SpecInfo rules (rulesRhsFreeVars rules)

extendSpecInfo :: SpecInfo -> [CoreRule] -> SpecInfo
extendSpecInfo (SpecInfo rs1 fvs1) rs2
  = SpecInfo (rs2 ++ rs1) (rulesRhsFreeVars rs2 `unionVarSet` fvs1)

addSpecInfo :: SpecInfo -> SpecInfo -> SpecInfo
addSpecInfo (SpecInfo rs1 fvs1) (SpecInfo rs2 fvs2) 
  = SpecInfo (rs1 ++ rs2) (fvs1 `unionVarSet` fvs2)

addIdSpecialisations :: Id -> [CoreRule] -> Id
addIdSpecialisations id rules
  = setIdSpecialisation id $
    extendSpecInfo (idSpecialisation id) rules

rulesOfBinds :: [CoreBind] -> [CoreRule]
rulesOfBinds binds = concatMap (concatMap idCoreRules . bindersOf) binds
\end{code}


%************************************************************************
%*									*
		RuleBase
%*									*
%************************************************************************

\begin{code}
type RuleBase = NameEnv [CoreRule]
	-- Maps (the name of) an Id to its rules
	-- The rules are are unordered; 
	-- we sort out any overlaps on lookup

emptyRuleBase = emptyNameEnv

mkRuleBase :: [CoreRule] -> RuleBase
mkRuleBase rules = extendRuleBaseList emptyRuleBase rules

extendRuleBaseList :: RuleBase -> [CoreRule] -> RuleBase
extendRuleBaseList rule_base new_guys
  = foldl extendRuleBase rule_base new_guys

unionRuleBase :: RuleBase -> RuleBase -> RuleBase
unionRuleBase rb1 rb2 = plusNameEnv_C (++) rb1 rb2

extendRuleBase :: RuleBase -> CoreRule -> RuleBase
extendRuleBase rule_base rule
  = extendNameEnv_Acc (:) singleton rule_base (ruleIdName rule) rule

pprRuleBase :: RuleBase -> SDoc
pprRuleBase rules = vcat [ pprRules (tidyRules emptyTidyEnv rs) 
			 | rs <- nameEnvElts rules ]
\end{code}


%************************************************************************
%*									*
\subsection{Matching}
%*									*
%************************************************************************

\begin{code}
lookupRule :: (Activation -> Bool) -> InScopeSet
	   -> RuleBase	-- Imported rules
	   -> Id -> [CoreExpr] -> Maybe (RuleName, CoreExpr)
lookupRule is_active in_scope rule_base fn args
  = matchRules is_active in_scope fn args rules
  where
	-- The rules for an Id come from two places:
	--	(a) the ones it is born with (idCoreRules fn)
	--	(b) rules added in subsequent modules (extra_rules)
	-- PrimOps, for example, are born with a bunch of rules under (a)
    rules = extra_rules ++ idCoreRules fn
    extra_rules | isLocalId fn = []
	  	| otherwise    = lookupNameEnv rule_base (idName fn) `orElse` []

matchRules :: (Activation -> Bool) -> InScopeSet
	   -> Id -> [CoreExpr]
	   -> [CoreRule] -> Maybe (RuleName, CoreExpr)
-- See comments on matchRule
matchRules is_active in_scope fn args rules
  = case go [] rules of
	[]     -> Nothing
	(m:ms) -> Just (case findBest (fn,args) m ms of
			  (rule, ans) -> (ru_name rule, ans))
  where
    rough_args = map roughTopName args

    go :: [(CoreRule,CoreExpr)] -> [CoreRule] -> [(CoreRule,CoreExpr)]
    go ms []	       = ms
    go ms (r:rs) = case (matchRule is_active in_scope args rough_args r) of
			Just e  -> go ((r,e):ms) rs
			Nothing -> go ms         rs

findBest :: (Id, [CoreExpr])
	 -> (CoreRule,CoreExpr) -> [(CoreRule,CoreExpr)] -> (CoreRule,CoreExpr)
-- All these pairs matched the expression
-- Return the pair the the most specific rule
-- The (fn,args) is just for overlap reporting

findBest target (rule,ans)   [] = (rule,ans)
findBest target (rule1,ans1) ((rule2,ans2):prs)
  | rule1 `isMoreSpecific` rule2 = findBest target (rule1,ans1) prs
  | rule2 `isMoreSpecific` rule1 = findBest target (rule2,ans2) prs
#ifdef DEBUG
  | otherwise = pprTrace "Rules.findBest: rule overlap (Rule 1 wins)"
			 (vcat [ptext SLIT("Expression to match:") <+> ppr fn <+> sep (map ppr args),
				ptext SLIT("Rule 1:") <+> ppr rule1, 
				ptext SLIT("Rule 2:") <+> ppr rule2]) $
		findBest target (rule1,ans1) prs
#else
  | otherwise = findBest target (rule1,ans1) prs
#endif
  where
    (fn,args) = target

isMoreSpecific :: CoreRule -> CoreRule -> Bool
isMoreSpecific (BuiltinRule {}) r2 = True
isMoreSpecific r1 (BuiltinRule {}) = False
isMoreSpecific (Rule { ru_bndrs = bndrs1, ru_args = args1 })
	       (Rule { ru_bndrs = bndrs2, ru_args = args2 })
  = isJust (matchN in_scope bndrs2 args2 args1)
  where
   in_scope = mkInScopeSet (mkVarSet bndrs1)
	-- Actually we should probably include the free vars 
	-- of rule1's args, but I can't be bothered

noBlackList :: Activation -> Bool
noBlackList act = False		-- Nothing is black listed

matchRule :: (Activation -> Bool) -> InScopeSet
	  -> [CoreExpr] -> [Maybe Name]
	  -> CoreRule -> Maybe CoreExpr

-- If (matchRule rule args) returns Just (name,rhs)
-- then (f args) matches the rule, and the corresponding
-- rewritten RHS is rhs
--
-- The bndrs and rhs is occurrence-analysed
--
-- 	Example
--
-- The rule
--	forall f g x. map f (map g x) ==> map (f . g) x
-- is stored
--	CoreRule "map/map" 
--		 [f,g,x]		-- tpl_vars
--		 [f,map g x]		-- tpl_args
--		 map (f.g) x)		-- rhs
--	  
-- Then the call: matchRule the_rule [e1,map e2 e3]
--	  = Just ("map/map", (\f,g,x -> rhs) e1 e2 e3)
--
-- Any 'surplus' arguments in the input are simply put on the end
-- of the output.

matchRule is_active in_scope args rough_args
	  (BuiltinRule { ru_name = name, ru_try = match_fn })
  = case match_fn args of
	Just expr -> Just expr
	Nothing   -> Nothing

matchRule is_active in_scope args rough_args
          (Rule { ru_name = rn, ru_act = act, ru_rough = tpl_tops,
		  ru_bndrs = tpl_vars, ru_args = tpl_args,
		  ru_rhs = rhs })
  | not (is_active act)		      = Nothing
  | ruleCantMatch tpl_tops rough_args = Nothing
  | otherwise
  = case matchN in_scope tpl_vars tpl_args args of
	Nothing			   -> Nothing
	Just (binds, tpl_vals, leftovers) -> Just (mkLets binds $
						   rule_fn
						    `mkApps` tpl_vals
						    `mkApps` leftovers)
  where
    rule_fn = occurAnalyseExpr (mkLams tpl_vars rhs)
	-- We could do this when putting things into the rulebase, I guess
\end{code}

\begin{code}
matchN	:: InScopeSet
	-> [Var]		-- Template tyvars
	-> [CoreExpr]		-- Template
	-> [CoreExpr]		-- Target; can have more elts than template
	-> Maybe ([CoreBind],	-- Bindings to wrap around the entire result
		  [CoreExpr], 	-- What is substituted for each template var
		  [CoreExpr])	-- Leftover target exprs

matchN in_scope tmpl_vars tmpl_es target_es
  = do	{ ((tv_subst, id_subst, binds), leftover_es)
		<- go init_menv emptySubstEnv tmpl_es target_es
	; return (fromOL binds, 
		  map (lookup_tmpl tv_subst id_subst) tmpl_vars, 
		  leftover_es) }
  where
    init_menv = ME { me_tmpls = mkVarSet tmpl_vars, me_env = init_rn_env }
    init_rn_env = mkRnEnv2 (extendInScopeSetList in_scope tmpl_vars)
		
    go menv subst []     es 	= Just (subst, es)
    go menv subst ts     [] 	= Nothing	-- Fail if too few actual args
    go menv subst (t:ts) (e:es) = do { subst1 <- match menv subst t e 
				     ; go menv subst1 ts es }

    lookup_tmpl :: TvSubstEnv -> IdSubstEnv -> Var -> CoreExpr
    lookup_tmpl tv_subst id_subst tmpl_var
	| isTyVar tmpl_var = case lookupVarEnv tv_subst tmpl_var of
				Just ty 	-> Type ty
				Nothing 	-> unbound tmpl_var
	| otherwise	   = case lookupVarEnv id_subst tmpl_var of
				Just e -> e
				other  -> unbound tmpl_var
 
    unbound var = pprPanic "Template variable unbound in rewrite rule" (ppr var)
\end{code}


	---------------------------------------------
		The inner workings of matching
	---------------------------------------------

\begin{code}
-- These two definitions are not the same as in Subst,
-- but they simple and direct, and purely local to this module
--
-- * The domain of the TvSubstEnv and IdSubstEnv are the template
--   variables passed into the match.
--
-- * The (OrdList CoreBind) in a SubstEnv are the bindings floated out
--   from nested matches; see the Let case of match, below
--
type SubstEnv   = (TvSubstEnv, IdSubstEnv, OrdList CoreBind)
type IdSubstEnv = IdEnv CoreExpr		

emptySubstEnv :: SubstEnv
emptySubstEnv = (emptyVarEnv, emptyVarEnv, nilOL)


--	At one stage I tried to match even if there are more 
--	template args than real args.

--	I now think this is probably a bad idea.
--	Should the template (map f xs) match (map g)?  I think not.
--	For a start, in general eta expansion wastes work.
--	SLPJ July 99


match :: MatchEnv
      -> SubstEnv
      -> CoreExpr		-- Template
      -> CoreExpr		-- Target
      -> Maybe SubstEnv

-- See the notes with Unify.match, which matches types
-- Everything is very similar for terms

-- Interesting examples:
-- Consider matching
--	\x->f 	   against    \f->f
-- When we meet the lambdas we must remember to rename f to f' in the
-- second expresion.  The RnEnv2 does that.
--
-- Consider matching 
--	forall a. \b->b	   against   \a->3
-- We must rename the \a.  Otherwise when we meet the lambdas we 
-- might substitute [a/b] in the template, and then erroneously 
-- succeed in matching what looks like the template variable 'a' against 3.

-- The Var case follows closely what happens in Unify.match
match menv subst (Var v1) e2 
  | Just subst <- match_var menv subst v1 e2
  = Just subst

-- Here is another important rule: if the term being matched is a
-- variable, we expand it so long as its unfolding is a WHNF
-- (Its occurrence information is not necessarily up to date,
--  so we don't use it.)
match menv subst e1 (Var v2)
  | not (inRnEnvR rn_env v2),
	-- If v2 is in the RnEnvR, then it's locally bound and can't
	-- have an unfolding. We must make this check because if it
	-- is locally bound we must not look it up in the in-scope set
	-- E.g. 	(\x->x) where x is already in scope
    isCheapUnfolding unfolding
  = match menv subst e1 (unfoldingTemplate unfolding)
  where
    rn_env    = me_env menv
    unfolding = idUnfolding (lookupRnInScope rn_env v2)
	-- Notice that we look up v2 in the in-scope set
	-- See Note [Lookup in-scope]

match menv subst (Lit lit1) (Lit lit2)
  | lit1 == lit2
  = Just subst

match menv subst (App f1 a1) (App f2 a2)
  = do 	{ subst' <- match menv subst f1 f2
	; match menv subst' a1 a2 }

match menv subst (Lam x1 e1) (Lam x2 e2)
  = match menv' subst e1 e2
  where
    menv' = menv { me_env = rnBndr2 (me_env menv) x1 x2 }

-- This rule does eta expansion
--		(\x.M)  ~  N 	iff	M  ~  N x
match menv subst (Lam x1 e1) e2
  = match menv' subst e1 (App e2 (varToCoreExpr new_x))
  where
    (rn_env', new_x) = rnBndrL (me_env menv) x1
    menv' = menv { me_env = rn_env' }

-- Eta expansion the other way
--	M  ~  (\y.N)	iff   M	y     ~  N
match menv subst e1 (Lam x2 e2)
  = match menv' subst (App e1 (varToCoreExpr new_x)) e2
  where
    (rn_env', new_x) = rnBndrR (me_env menv) x2
    menv' = menv { me_env = rn_env' }

match menv subst (Case e1 x1 ty1 alts1) (Case e2 x2 ty2 alts2)
  = do	{ subst1 <- match_ty menv subst ty1 ty2
	; subst2 <- match menv subst1 e1 e2
	; let menv' = menv { me_env = rnBndr2 (me_env menv) x1 x2 }
	; match_alts menv' subst2 alts1 alts2	-- Alts are both sorted
	}

match menv subst (Type ty1) (Type ty2)
  = match_ty menv subst ty1 ty2

match menv subst (Note (Coerce to1 from1) e1) (Note (Coerce to2 from2) e2)
  = do	{ subst1 <- match_ty menv subst  to1   to2
	; subst2 <- match_ty menv subst1 from1 from2
	; match menv subst2 e1 e2 }

-- Matching a let-expression.  Consider
--	RULE forall x.  f (g x) = <rhs>
-- and target expression
--	f (let { w=R } in g E))
-- Then we'd like the rule to match, to generate
--	let { w=R } in (\x. <rhs>) E
-- In effect, we want to float the let-binding outward, to enable
-- the match to happen.  This is the WHOLE REASON for accumulating
-- bindings in the SubstEnv
--
-- We can only do this if
--	(a) Widening the scope of w does not capture any variables
--	    We use a conservative test: w is not already in scope
--	(b) The free variables of R are not bound by the part of the
--	    target expression outside the let binding; e.g.
--		f (\v. let w = v+1 in g E)
--	    Here we obviously cannot float the let-binding for w.

match menv subst@(tv_subst, id_subst, binds) e1 (Let bind e2)
  | all freshly_bound bndrs,
    not (any locally_bound bind_fvs)
  = match (menv { me_env = rn_env' }) 
	  (tv_subst, id_subst, binds `snocOL` bind)
	  e1 e2
  where
    rn_env   = me_env menv
    bndrs    = bindersOf bind
    bind_fvs = varSetElems (bindFreeVars bind)
    freshly_bound x = not (x `rnInScope` rn_env)
    locally_bound x = inRnEnvR rn_env x
    rn_env' = extendRnInScopeList rn_env bndrs

-- This is an interesting rule: we simply ignore lets in the 
-- term being matched against!  The unfolding inside it is (by assumption)
-- already inside any occurrences of the bound variables, so we'll expand
-- them when we encounter them.  This gives a chance of matching
--	forall x,y.  f (g (x,y))
-- against
--	f (let v = (a,b) in g v)

match menv subst e1 (Let bind e2)
  = match (menv { me_env = rn_env' }) subst e1 e2
  where
    (rn_env', _bndrs') = mapAccumL rnBndrR (me_env menv) (bindersOf bind)
	-- It's important to do this renaming, so that the bndrs
	-- are brought into the local scope. For example:
	-- Matching
	--	forall f,x,xs. f (x:xs)
	--   against
	--	f (let y = e in (y:[]))
	-- We must not get success with x->y!  So we record that y is
	-- locally bound (with rnBndrR), and proceed.  The Var case
	-- will fail when trying to bind x->y
	--

-- Everything else fails
match menv subst e1 e2 = Nothing

------------------------------------------
match_var :: MatchEnv
      	  -> SubstEnv
      	  -> Var		-- Template
      	  -> CoreExpr		-- Target
      	  -> Maybe SubstEnv
match_var menv subst@(tv_subst, id_subst, binds) v1 e2
  | v1' `elemVarSet` me_tmpls menv
  = case lookupVarEnv id_subst v1' of
	Nothing	| any (inRnEnvR rn_env) (varSetElems (exprFreeVars e2))
		-> Nothing	-- Occurs check failure
		-- e.g. match forall a. (\x-> a x) against (\y. y y)

		| otherwise	-- No renaming to do on e2
		-> Just (tv_subst, extendVarEnv id_subst v1 e2, binds)

	Just e2' | tcEqExprX (nukeRnEnvL rn_env) e2' e2 
		 -> Just subst

		 | otherwise
		 -> Nothing

  | otherwise	-- v1 is not a template variable; check for an exact match with e2
  = case e2 of
       Var v2 | v1' == rnOccR rn_env v2 -> Just subst
       other				-> Nothing

  where
    rn_env = me_env menv
    v1'    = rnOccL rn_env v1	
	-- If the template is
	--	forall x. f x (\x -> x) = ...
	-- Then the x inside the lambda isn't the 
	-- template x, so we must rename first!
				

------------------------------------------
match_alts :: MatchEnv
      -> SubstEnv
      -> [CoreAlt]		-- Template
      -> [CoreAlt]		-- Target
      -> Maybe SubstEnv
match_alts menv subst [] []
  = return subst
match_alts menv subst ((c1,vs1,r1):alts1) ((c2,vs2,r2):alts2)
  | c1 == c2
  = do	{ subst1 <- match menv' subst r1 r2
	; match_alts menv subst1 alts1 alts2 }
  where
    menv' :: MatchEnv
    menv' = menv { me_env = rnBndrs2 (me_env menv) vs1 vs2 }

match_alts menv subst alts1 alts2 
  = Nothing
\end{code}

Matching Core types: use the matcher in TcType.
Notice that we treat newtypes as opaque.  For example, suppose 
we have a specialised version of a function at a newtype, say 
	newtype T = MkT Int
We only want to replace (f T) with f', not (f Int).

\begin{code}
------------------------------------------
match_ty menv (tv_subst, id_subst, binds) ty1 ty2
  = do	{ tv_subst' <- Unify.ruleMatchTyX menv tv_subst ty1 ty2
	; return (tv_subst', id_subst, binds) }
\end{code}


Note [Lookup in-scope]
~~~~~~~~~~~~~~~~~~~~~~
Consider this example
	foo :: Int -> Maybe Int -> Int
	foo 0 (Just n) = n
	foo m (Just n) = foo (m-n) (Just n)

SpecConstr sees this fragment:

	case w_smT of wild_Xf [Just A] {
	  Data.Maybe.Nothing -> lvl_smf;
	  Data.Maybe.Just n_acT [Just S(L)] ->
	    case n_acT of wild1_ams [Just A] { GHC.Base.I# y_amr [Just L] ->
	    $wfoo_smW (GHC.Prim.-# ds_Xmb y_amr) wild_Xf
	    }};

and correctly generates the rule

	RULES: "SC:$wfoo1" [0] __forall {y_amr [Just L] :: GHC.Prim.Int#
					  sc_snn :: GHC.Prim.Int#}
	  $wfoo_smW sc_snn (Data.Maybe.Just @ GHC.Base.Int (GHC.Base.I# y_amr))
	  = $s$wfoo_sno y_amr sc_snn ;]

BUT we must ensure that this rule matches in the original function!
Note that the call to $wfoo is
	    $wfoo_smW (GHC.Prim.-# ds_Xmb y_amr) wild_Xf

During matching we expand wild_Xf to (Just n_acT).  But then we must also
expand n_acT to (I# y_amr).  And we can only do that if we look up n_acT
in the in-scope set, because in wild_Xf's unfolding it won't have an unfolding
at all. 

That is why the 'lookupRnInScope' call in the (Var v2) case of 'match'
is so important.


%************************************************************************
%*									*
\subsection{Checking a program for failing rule applications}
%*									*
%************************************************************************

-----------------------------------------------------
			Game plan
-----------------------------------------------------

We want to know what sites have rules that could have fired but didn't.
This pass runs over the tree (without changing it) and reports such.

NB: we assume that this follows a run of the simplifier, so every Id
occurrence (including occurrences of imported Ids) is decorated with
all its (active) rules.  No need to construct a rule base or anything
like that.

\begin{code}
ruleCheckProgram :: CompilerPhase -> String -> [CoreBind] -> SDoc
-- Report partial matches for rules beginning 
-- with the specified string
ruleCheckProgram phase rule_pat binds 
  | isEmptyBag results
  = text "Rule check results: no rule application sites"
  | otherwise
  = vcat [text "Rule check results:",
	  line,
	  vcat [ p $$ line | p <- bagToList results ]
	 ]
  where
    results = unionManyBags (map (ruleCheckBind (phase, rule_pat)) binds)
    line = text (replicate 20 '-')
	  
type RuleCheckEnv = (CompilerPhase, String) 	-- Phase and Pattern

ruleCheckBind :: RuleCheckEnv -> CoreBind -> Bag SDoc
   -- The Bag returned has one SDoc for each call site found
ruleCheckBind env (NonRec b r) = ruleCheck env r
ruleCheckBind env (Rec prs)    = unionManyBags [ruleCheck env r | (b,r) <- prs]

ruleCheck :: RuleCheckEnv -> CoreExpr -> Bag SDoc
ruleCheck env (Var v) 	    = emptyBag
ruleCheck env (Lit l) 	    = emptyBag
ruleCheck env (Type ty)     = emptyBag
ruleCheck env (App f a)     = ruleCheckApp env (App f a) []
ruleCheck env (Note n e)    = ruleCheck env e
ruleCheck env (Let bd e)    = ruleCheckBind env bd `unionBags` ruleCheck env e
ruleCheck env (Lam b e)     = ruleCheck env e
ruleCheck env (Case e _ _ as) = ruleCheck env e `unionBags` 
			        unionManyBags [ruleCheck env r | (_,_,r) <- as]

ruleCheckApp env (App f a) as = ruleCheck env a `unionBags` ruleCheckApp env f (a:as)
ruleCheckApp env (Var f) as   = ruleCheckFun env f as
ruleCheckApp env other as     = ruleCheck env other
\end{code}

\begin{code}
ruleCheckFun :: RuleCheckEnv -> Id -> [CoreExpr] -> Bag SDoc
-- Produce a report for all rules matching the predicate
-- saying why it doesn't match the specified application

ruleCheckFun (phase, pat) fn args
  | null name_match_rules = emptyBag
  | otherwise		  = unitBag (ruleAppCheck_help phase fn args name_match_rules)
  where
    name_match_rules = filter match (idCoreRules fn)
    match rule = pat `isPrefixOf` unpackFS (ruleName rule)

ruleAppCheck_help :: CompilerPhase -> Id -> [CoreExpr] -> [CoreRule] -> SDoc
ruleAppCheck_help phase fn args rules
  = 	-- The rules match the pattern, so we want to print something
    vcat [text "Expression:" <+> ppr (mkApps (Var fn) args),
	  vcat (map check_rule rules)]
  where
    n_args = length args
    i_args = args `zip` [1::Int ..]
    rough_args = map roughTopName args

    check_rule rule = rule_herald rule <> colon <+> rule_info rule

    rule_herald (BuiltinRule { ru_name = name })
	= ptext SLIT("Builtin rule") <+> doubleQuotes (ftext name)
    rule_herald (Rule { ru_name = name })
	= ptext SLIT("Rule") <+> doubleQuotes (ftext name)

    rule_info rule
	| Just _ <- matchRule noBlackList emptyInScopeSet args rough_args rule
	= text "matches (which is very peculiar!)"

    rule_info (BuiltinRule {}) = text "does not match"

    rule_info (Rule { ru_name = name, ru_act = act, 
		      ru_bndrs = rule_bndrs, ru_args = rule_args})
	| not (isActive phase act)    = text "active only in later phase"
	| n_args < n_rule_args	      = text "too few arguments"
	| n_mismatches == n_rule_args = text "no arguments match"
	| n_mismatches == 0	      = text "all arguments match (considered individually), but rule as a whole does not"
	| otherwise		      = text "arguments" <+> ppr mismatches <+> text "do not match (1-indexing)"
	where
	  n_rule_args  = length rule_args
	  n_mismatches = length mismatches
	  mismatches   = [i | (rule_arg, (arg,i)) <- rule_args `zip` i_args,
			      not (isJust (match_fn rule_arg arg))]

	  lhs_fvs = exprsFreeVars rule_args	-- Includes template tyvars
	  match_fn rule_arg arg = match menv emptySubstEnv rule_arg arg
		where
		  in_scope = lhs_fvs `unionVarSet` exprFreeVars arg
		  menv = ME { me_env   = mkRnEnv2 (mkInScopeSet in_scope)
			    , me_tmpls = mkVarSet rule_bndrs }
\end{code}


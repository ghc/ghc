%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreRules]{Transformation rules}

\begin{code}
module Rules (
	RuleBase, emptyRuleBase, 
	extendRuleBase, extendRuleBaseList, addRuleBaseFVs, 
	ruleBaseIds, ruleBaseFVs,
	pprRuleBase, ruleCheckProgram,

        lookupRule, addRule, addIdSpecialisations
    ) where

#include "HsVersions.h"

import CoreSyn		-- All of it
import OccurAnal	( occurAnalyseRule )
import CoreFVs		( exprFreeVars, ruleRhsFreeVars, ruleLhsFreeIds )
import CoreUnfold	( isCheapUnfolding, unfoldingTemplate )
import CoreUtils	( eqExpr )
import CoreTidy		( pprTidyIdRules )
import Subst		( Subst, InScopeSet, mkInScopeSet, lookupSubst, extendSubst,
			  substEnv, setSubstEnv, emptySubst, isInScope, emptyInScopeSet,
			  bindSubstList, unBindSubstList, substInScope, uniqAway
			)
import Id		( Id, idUnfolding, idSpecialisation, setIdSpecialisation ) 
import Var		( isId )
import VarSet
import VarEnv
import TcType		( mkTyVarTy )
import qualified TcType ( match )
import BasicTypes	( Activation, CompilerPhase, isActive )

import Outputable
import FastString
import Maybe		( isJust, isNothing, fromMaybe )
import Util		( sortLt )
import Bag
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


%************************************************************************
%*									*
\subsection{Matching}
%*									*
%************************************************************************

\begin{code}
matchRules :: (Activation -> Bool) -> InScopeSet
	   -> [CoreRule] -> [CoreExpr] -> Maybe (RuleName, CoreExpr)
-- See comments on matchRule
matchRules is_active in_scope [] args = Nothing
matchRules is_active in_scope (rule:rules) args
  = case matchRule is_active in_scope rule args of
	Just result -> Just result
	Nothing	    -> matchRules is_active in_scope rules args

noBlackList :: Activation -> Bool
noBlackList act = False		-- Nothing is black listed

matchRule :: (Activation -> Bool) -> InScopeSet
	  -> CoreRule -> [CoreExpr] -> Maybe (RuleName, CoreExpr)

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
--
-- ASSUMPTION (A):
--	A1. No top-level variable is bound in the target
--	A2. No template variable  is bound in the target
--	A3. No lambda bound template variable is free in any subexpression of the target
--
-- To see why A1 is necessary, consider matching
--	\x->f 	   against    \f->f
-- When we meet the lambdas we substitute [f/x] in the template (a no-op),
-- and then erroneously succeed in matching f against f.
--
-- To see why A2 is needed consider matching 
--	forall a. \b->b	   against   \a->3
-- When we meet the lambdas we substitute [a/b] in the template, and then
-- erroneously succeed in matching what looks like the template variable 'a' against 3.
--
-- A3 is needed to validate the rule that says
--	(\x->E) matches F
-- if
--	(\x->E)	matches (\x->F x)


matchRule is_active in_scope rule@(BuiltinRule name match_fn) args
  = case match_fn args of
	Just expr -> Just (name,expr)
	Nothing   -> Nothing

matchRule is_active in_scope rule@(Rule rn act tpl_vars tpl_args rhs) args
  | not (is_active act)
  = Nothing
  | otherwise
  = go tpl_args args emptySubst
	-- We used to use the in_scope set, but I don't think that's necessary
	-- After all, the result is going to be simplified again with that in_scope set
 where
   tpl_var_set = mkVarSet tpl_vars

   -----------------------
	-- Do the business
   go (tpl_arg:tpl_args) (arg:args) subst = match tpl_arg arg tpl_var_set (go tpl_args args) subst

	-- Two easy ways to terminate
   go [] []	    subst = Just (rn, app_match subst (mkLams tpl_vars rhs) tpl_vars)
   go [] args	    subst = Just (rn, app_match subst (mkLams tpl_vars rhs) tpl_vars `mkApps` args)

	-- One tiresome way to terminate: check for excess unmatched
	-- template arguments
   go tpl_args []   subst = Nothing	-- Failure


   -----------------------
   app_match subst fn vs = foldl go fn vs
	where	
	  senv    = substEnv subst
	  go fn v = case lookupSubstEnv senv v of
			Just (DoneEx ex)  -> fn `App` ex 
			Just (DoneTy ty)  -> fn `App` Type ty
			-- Substitution should bind them all!


   -----------------------
{-	The code below tries to match even if there are more 
	template args than real args.

	I now think this is probably a bad idea.
	Should the template (map f xs) match (map g)?  I think not.
	For a start, in general eta expansion wastes work.
	SLPJ July 99

      = case eta_complete tpl_args (mkVarSet leftovers) of
	    Just leftovers' -> Just (rn, mkLams done (mkLams leftovers' rhs), 
				     mk_result_args subst done)
	    Nothing	    -> Nothing	-- Failure
      where
	(done, leftovers) = partition (\v -> isJust (lookupSubstEnv subst_env v))
				      (map zapOccInfo tpl_vars)
		-- Zap the occ info 
	subst_env = substEnv subst
   						
   -----------------------
   eta_complete [] vars = ASSERT( isEmptyVarSet vars )
			  Just []
   eta_complete (Type ty:tpl_args) vars
	= case getTyVar_maybe ty of
		Just tv |  tv `elemVarSet` vars
			-> case eta_complete tpl_args (vars `delVarSet` tv) of
				Just vars' -> Just (tv:vars')
				Nothing    -> Nothing
		other   -> Nothing

   eta_complete (Var v:tpl_args) vars
	| v `elemVarSet` vars
	= case eta_complete tpl_args (vars `delVarSet` v) of
		Just vars' -> Just (v:vars')
		Nothing    -> Nothing

   eta_complete other vars = Nothing


zapOccInfo bndr | isTyVar bndr = bndr
		| otherwise    = zapLamIdInfo bndr
-}
\end{code}

\begin{code}
type Matcher result =  VarSet			-- Template variables
	     	    -> (Subst -> Maybe result)	-- Continuation if success
		    -> Subst  -> Maybe result	-- Substitution so far -> result
-- The *SubstEnv* in these Substs apply to the TEMPLATE only 

-- The *InScopeSet* in these Substs gives variables bound so far in the
--	target term.  So when matching forall a. (\x. a x) against (\y. y y)
--	while processing the body of the lambdas, the in-scope set will be {y}.
--	That lets us do the occurs-check when matching 'a' against 'y'

match :: CoreExpr		-- Template
      -> CoreExpr		-- Target
      -> Matcher result

match_fail = Nothing

match (Var v1) e2 tpl_vars kont subst
  = case lookupSubst subst v1 of
	Nothing	| v1 `elemVarSet` tpl_vars  	-- v1 is a template variable
		-> if (any (`isInScope` subst) (varSetElems (exprFreeVars e2))) then
			 match_fail		-- Occurs check failure
						-- e.g. match forall a. (\x-> a x) against (\y. y y)
		   else
			 kont (extendSubst subst v1 (DoneEx e2))


		| eqExpr (Var v1) e2	   -> kont subst
			-- v1 is not a template variable, so it must be a global constant

	Just (DoneEx e2')  | eqExpr e2' e2 -> kont subst

	other -> match_fail

match (Lit lit1) (Lit lit2) tpl_vars kont subst
  | lit1 == lit2
  = kont subst

match (App f1 a1) (App f2 a2) tpl_vars kont subst
  = match f1 f2 tpl_vars (match a1 a2 tpl_vars kont) subst

match (Lam x1 e1) (Lam x2 e2) tpl_vars kont subst
  = bind [x1] [x2] (match e1 e2) tpl_vars kont subst

-- This rule does eta expansion
--		(\x.M)  ~  N 	iff	M  ~  N x
-- See assumption A3
match (Lam x1 e1) e2 tpl_vars kont subst
  = bind [x1] [x1] (match e1 (App e2 (mkVarArg x1))) tpl_vars kont subst

-- Eta expansion the other way
--	M  ~  (\y.N)	iff   \y.M y  ~  \y.N
--			iff   M	y     ~  N
-- Remembering that by (A), y can't be free in M, we get this
match e1 (Lam x2 e2) tpl_vars kont subst
  = bind [new_id] [x2] (match (App e1 (mkVarArg new_id)) e2) tpl_vars kont subst
  where
    new_id = uniqAway (substInScope subst) x2
	-- This uniqAway is actually needed.  Here's the example:
	--  rule:	foldr (mapFB (:) f) [] = mapList
	--  target:	foldr (\x. mapFB k f x) []
	--	      where
	--		k = \x. mapFB ... x
	-- The first \x is ok, but when we inline k, hoping it might
	-- match (:) we find a second \x.

match (Case e1 x1 alts1) (Case e2 x2 alts2) tpl_vars kont subst
  = match e1 e2 tpl_vars case_kont subst
  where
    case_kont subst = bind [x1] [x2] (match_alts alts1 (sortLt lt_alt alts2))
				     tpl_vars kont subst

match (Type ty1) (Type ty2) tpl_vars kont subst
  = match_ty ty1 ty2 tpl_vars kont subst

match (Note (Coerce to1 from1) e1) (Note (Coerce to2 from2) e2)
      tpl_vars kont subst
  = (match_ty to1   to2   tpl_vars $
     match_ty from1 from2 tpl_vars $
     match e1 e2 tpl_vars kont) subst


{-	I don't buy this let-rule any more
	The let rule fails on matching
		forall f,x,xs. f (x:xs)
	against
		f (let y = e in (y:[]))
	because we just get x->y, which is bogus.

-- This is an interesting rule: we simply ignore lets in the 
-- term being matched against!  The unfolding inside it is (by assumption)
-- already inside any occurrences of the bound variables, so we'll expand
-- them when we encounter them.  Meanwhile, we can't get false matches because
-- (also by assumption) the term being matched has no shadowing.
match e1 (Let bind e2) tpl_vars kont subst
  = match e1 e2 tpl_vars kont subst
-}

-- Here is another important rule: if the term being matched is a
-- variable, we expand it so long as its unfolding is a WHNF
-- (Its occurrence information is not necessarily up to date,
--  so we don't use it.)
match e1 (Var v2) tpl_vars kont subst
  | isCheapUnfolding unfolding
  = match e1 (unfoldingTemplate unfolding) tpl_vars kont subst
  where
    unfolding = idUnfolding v2


-- We can't cope with lets in the template

match e1 e2 tpl_vars kont subst = match_fail


------------------------------------------
match_alts [] [] tpl_vars kont subst
  = kont subst
match_alts ((c1,vs1,r1):alts1) ((c2,vs2,r2):alts2) tpl_vars kont subst
  | c1 == c2
  = bind vs1 vs2 (match r1 r2) tpl_vars
		 (match_alts alts1 alts2 tpl_vars kont)
		 subst
match_alts alts1 alts2 tpl_vars kont subst = match_fail

lt_alt (con1, _, _) (con2, _, _) = con1 < con2

----------------------------------------
bind :: [CoreBndr]	-- Template binders
     -> [CoreBndr]	-- Target binders
     -> Matcher result
     -> Matcher result
-- This makes uses of assumption (A) above.  For example,
-- this would fail:
--	Template: (\x.y)	(y is free)
--	Target  : (\y.y)	(y is bound)
-- We rename x to y in the template... but then erroneously
-- match y against y.  But this can't happen because of (A)
bind vs1 vs2 matcher tpl_vars kont subst
  = WARN( not (all not_in_subst vs1), bug_msg )
    matcher tpl_vars kont' subst'
  where
    kont' subst'' = kont (unBindSubstList subst'' vs1 vs2)
    subst'        = bindSubstList subst vs1 vs2

	-- The unBindSubst relies on no shadowing in the template
    not_in_subst v = isNothing (lookupSubst subst v)
    bug_msg = sep [ppr vs1, ppr vs2]

----------------------------------------
matches [] [] tpl_vars kont subst 
  = kont subst
matches (e:es) (e':es') tpl_vars kont subst
  = match e e' tpl_vars (matches es es' tpl_vars kont) subst
matches es es' tpl_vars kont subst 
  = match_fail

----------------------------------------
mkVarArg :: CoreBndr -> CoreArg
mkVarArg v | isId v    = Var v
	   | otherwise = Type (mkTyVarTy v)
\end{code}

Matching Core types: use the matcher in TcType.
Notice that we treat newtypes as opaque.  For example, suppose 
we have a specialised version of a function at a newtype, say 
	newtype T = MkT Int
We only want to replace (f T) with f', not (f Int).

\begin{code}
----------------------------------------
match_ty ty1 ty2 tpl_vars kont subst
  = TcType.match ty1 ty2 tpl_vars kont' (substEnv subst)
  where
    kont' senv = kont (setSubstEnv subst senv) 
\end{code}



%************************************************************************
%*									*
\subsection{Adding a new rule}
%*									*
%************************************************************************

\begin{code}
addRule :: Id -> CoreRules -> CoreRule -> CoreRules

-- Add a new rule to an existing bunch of rules.
-- The rules are for the given Id; the Id argument is needed only
-- so that we can exclude the Id from its own RHS free-var set

-- Insert the new rule just before a rule that is *less specific*
-- than the new one; or at the end if there isn't such a one.
-- In this way we make sure that when looking up, the first match
-- is the most specific.
--
-- We make no check for rules that unify without one dominating
-- the other.   Arguably this would be a bug.

addRule id (Rules rules rhs_fvs) rule@(BuiltinRule _ _)
  = Rules (rule:rules) rhs_fvs
	-- Put it at the start for lack of anything better

addRule id (Rules rules rhs_fvs) rule
  = Rules (insertRule rules new_rule) (rhs_fvs `unionVarSet` new_rhs_fvs)
  where
    new_rule    = occurAnalyseRule rule
    new_rhs_fvs = ruleRhsFreeVars new_rule `delVarSet` id
	-- Hack alert!
	-- Don't include the Id in its own rhs free-var set.
	-- Otherwise the occurrence analyser makes bindings recursive
	-- that shoudn't be.  E.g.
	--	RULE:  f (f x y) z  ==>  f x (f y z)

insertRule rules new_rule@(Rule _ _ tpl_vars tpl_args _)
  = go rules
  where
    tpl_var_set = mkInScopeSet (mkVarSet tpl_vars)
	-- Actually we should probably include the free vars of tpl_args,
	-- but I can't be bothered

    go []					= [new_rule]
    go (rule:rules) | new_is_more_specific rule = (new_rule:rule:rules)
		    | otherwise		        = rule : go rules

    new_is_more_specific rule = isJust (matchRule noBlackList tpl_var_set rule tpl_args)

addIdSpecialisations :: Id -> [CoreRule] -> Id
addIdSpecialisations id rules
  = setIdSpecialisation id new_specs
  where
    new_specs = foldl (addRule id) (idSpecialisation id) rules
\end{code}


%************************************************************************
%*									*
\subsection{Looking up a rule}
%*									*
%************************************************************************

\begin{code}
lookupRule :: (Activation -> Bool) -> InScopeSet
	   -> Id -> [CoreExpr] -> Maybe (RuleName, CoreExpr)
lookupRule is_active in_scope fn args
  = case idSpecialisation fn of
	Rules rules _ -> matchRules is_active in_scope rules args
\end{code}


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
ruleCheck env (Case e _ as) = ruleCheck env e `unionBags` 
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
    name_match_rules = case idSpecialisation fn of
			  Rules rules _ -> filter match rules
    match rule = pat `isPrefixOf` unpackFS (ruleName rule)

ruleAppCheck_help :: CompilerPhase -> Id -> [CoreExpr] -> [CoreRule] -> SDoc
ruleAppCheck_help phase fn args rules
  = 	-- The rules match the pattern, so we want to print something
    vcat [text "Expression:" <+> ppr (mkApps (Var fn) args),
	  vcat (map check_rule rules)]
  where
    n_args = length args
    i_args = args `zip` [1::Int ..]

    check_rule rule = rule_herald rule <> colon <+> rule_info rule

    rule_herald (BuiltinRule name _) = 
	ptext SLIT("Builtin rule") <+> doubleQuotes (ftext name)
    rule_herald (Rule name _ _ _ _)  = 
	ptext SLIT("Rule") <+> doubleQuotes (ftext name)

    rule_info rule
	| Just (name,_) <- matchRule noBlackList emptyInScopeSet rule args
	= text "matches (which is very peculiar!)"

    rule_info (BuiltinRule name fn) = text "does not match"

    rule_info (Rule name act rule_bndrs rule_args _)
	| not (isActive phase act)    = text "active only in later phase"
	| n_args < n_rule_args	      = text "too few arguments"
	| n_mismatches == n_rule_args = text "no arguments match"
	| n_mismatches == 0	      = text "all arguments match (considered individually), but the rule as a whole does not"
	| otherwise		      = text "arguments" <+> ppr mismatches <+> text "do not match (1-indexing)"
	where
	  n_rule_args  = length rule_args
	  n_mismatches = length mismatches
	  mismatches   = [i | (rule_arg, (arg,i)) <- rule_args `zip` i_args,
			      not (isJust (match_fn rule_arg arg))]

	  bndr_set 	        = mkVarSet rule_bndrs
	  match_fn rule_arg arg = match rule_arg arg bndr_set (\s -> Just ()) emptySubst
\end{code}


%************************************************************************
%*									*
\subsection{Getting the rules ready}
%*									*
%************************************************************************

\begin{code}
data RuleBase = RuleBase
		    IdSet	-- Ids with their rules in their specialisations
				-- Held as a set, so that it can simply be the initial
				-- in-scope set in the simplifier

		    IdSet	-- Ids (whether local or imported) mentioned on 
				-- LHS of some rule; these should be black listed

	-- This representation is a bit cute, and I wonder if we should
	-- change it to use (IdEnv CoreRule) which seems a bit more natural

ruleBaseIds (RuleBase ids _) = ids
ruleBaseFVs (RuleBase _ fvs) = fvs

emptyRuleBase = RuleBase emptyVarSet emptyVarSet

addRuleBaseFVs :: RuleBase -> IdSet -> RuleBase
addRuleBaseFVs (RuleBase rules fvs) extra_fvs 
  = RuleBase rules (fvs `unionVarSet` extra_fvs)

extendRuleBaseList :: RuleBase -> [(Id,CoreRule)] -> RuleBase
extendRuleBaseList rule_base new_guys
  = foldl extendRuleBase rule_base new_guys

extendRuleBase :: RuleBase -> (Id,CoreRule) -> RuleBase
extendRuleBase (RuleBase rule_ids rule_fvs) (id, rule)
  = RuleBase (extendVarSet rule_ids new_id)
	     (rule_fvs `unionVarSet` extendVarSet lhs_fvs id)
  where
    new_id = setIdSpecialisation id (addRule id old_rules rule)

    old_rules = idSpecialisation (fromMaybe id (lookupVarSet rule_ids id))
	-- Get the old rules from rule_ids if the Id is already there, but
	-- if not, use the Id from the incoming rule.  If may be a PrimOpId,
	-- in which case it may have rules in its belly already.  Seems
	-- dreadfully hackoid.

    lhs_fvs = ruleLhsFreeIds rule
	-- Finds *all* the free Ids of the LHS, not just
	-- locally defined ones!!

pprRuleBase :: RuleBase -> SDoc
pprRuleBase (RuleBase rules _) = vcat [ pprTidyIdRules id | id <- varSetElems rules ]
\end{code}

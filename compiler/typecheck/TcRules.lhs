%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1993-1998
%

TcRules: Typechecking transformation rules

\begin{code}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcRules ( tcRules ) where

import HsSyn
import TcRnMonad
import TcSimplify
import TcMType
import TcType
import TcHsType
import TcExpr
import TcEnv
import TcEvidence( TcEvBinds(..) )
import Type
import Id
import Name
import SrcLoc
import Outputable
import FastString
import Data.List( partition )
\end{code}

Note [Typechecking rules]
~~~~~~~~~~~~~~~~~~~~~~~~~
We *infer* the typ of the LHS, and use that type to *check* the type of 
the RHS.  That means that higher-rank rules work reasonably well. Here's
an example (test simplCore/should_compile/rule2.hs) produced by Roman:

   foo :: (forall m. m a -> m b) -> m a -> m b
   foo f = ...

   bar :: (forall m. m a -> m a) -> m a -> m a
   bar f = ...

   {-# RULES "foo/bar" foo = bar #-}

He wanted the rule to typecheck.

Note [Simplifying RULE constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the LHS of transformation rules we only simplify only equalities,
but not dictionaries.  We want to keep dictionaries unsimplified, to
serve as the available stuff for the RHS of the rule.  We *do* want to
simplify equalities, however, to detect ill-typed rules that cannot be
applied.

Implementation: the TcSFlags carried by the TcSMonad controls the
amount of simplification, so simplifyRuleLhs just sets the flag
appropriately.

Example.  Consider the following left-hand side of a rule
	f (x == y) (y > z) = ...
If we typecheck this expression we get constraints
	d1 :: Ord a, d2 :: Eq a
We do NOT want to "simplify" to the LHS
	forall x::a, y::a, z::a, d1::Ord a.
	  f ((==) (eqFromOrd d1) x y) ((>) d1 y z) = ...
Instead we want	
	forall x::a, y::a, z::a, d1::Ord a, d2::Eq a.
	  f ((==) d2 x y) ((>) d1 y z) = ...

Here is another example:
	fromIntegral :: (Integral a, Num b) => a -> b
	{-# RULES "foo"  fromIntegral = id :: Int -> Int #-}
In the rule, a=b=Int, and Num Int is a superclass of Integral Int. But
we *dont* want to get
	forall dIntegralInt.
	   fromIntegral Int Int dIntegralInt (scsel dIntegralInt) = id Int
because the scsel will mess up RULE matching.  Instead we want
	forall dIntegralInt, dNumInt.
	  fromIntegral Int Int dIntegralInt dNumInt = id Int

Even if we have 
	g (x == y) (y == z) = ..
where the two dictionaries are *identical*, we do NOT WANT
	forall x::a, y::a, z::a, d1::Eq a
	  f ((==) d1 x y) ((>) d1 y z) = ...
because that will only match if the dict args are (visibly) equal.
Instead we want to quantify over the dictionaries separately.

In short, simplifyRuleLhs must *only* squash equalities, leaving
all dicts unchanged, with absolutely no sharing.  

Also note that we can't solve the LHS constraints in isolation:
Example   foo :: Ord a => a -> a
	  foo_spec :: Int -> Int
          {-# RULE "foo"  foo = foo_spec #-}
Here, it's the RHS that fixes the type variable

HOWEVER, under a nested implication things are different
Consider
  f :: (forall a. Eq a => a->a) -> Bool -> ...
  {-# RULES "foo" forall (v::forall b. Eq b => b->b).
       f b True = ...
    #-}
Here we *must* solve the wanted (Eq a) from the given (Eq a)
resulting from skolemising the agument type of g.  So we 
revert to SimplCheck when going under an implication.  


------------------------ So the plan is this -----------------------

* Step 1: Simplify the LHS and RHS constraints all together in one bag
          We do this to discover all unification equalities

* Step 2: Zonk the ORIGINAL lhs constraints, and partition them into
          the ones we will quantify over, and the others

* Step 3: Decide on the type variables to quantify over

* Step 4: Simplify the LHS and RHS constraints separately, using the
          quantified constraints as givens


\begin{code}
tcRules :: [LRuleDecl Name] -> TcM [LRuleDecl TcId]
tcRules decls = mapM (wrapLocM tcRule) decls

tcRule :: RuleDecl Name -> TcM (RuleDecl TcId)
tcRule (HsRule name act hs_bndrs lhs fv_lhs rhs fv_rhs)
  = addErrCtxt (ruleCtxt name)	$
    do { traceTc "---- Rule ------" (ppr name)

    	-- Note [Typechecking rules]
       ; vars <- tcRuleBndrs hs_bndrs
       ; let (id_bndrs, tv_bndrs) = partition isId vars
       ; (lhs', lhs_wanted, rhs', rhs_wanted, rule_ty)
            <- tcExtendTyVarEnv tv_bndrs $
               tcExtendIdEnv    id_bndrs $
               do { ((lhs', rule_ty), lhs_wanted) <- captureConstraints (tcInferRho lhs)
                  ; (rhs', rhs_wanted) <- captureConstraints (tcMonoExpr rhs rule_ty)
                  ; return (lhs', lhs_wanted, rhs', rhs_wanted, rule_ty) }

       ; (lhs_evs, other_lhs_wanted) <- simplifyRule name lhs_wanted rhs_wanted

	-- Now figure out what to quantify over
	-- c.f. TcSimplify.simplifyInfer
	-- We quantify over any tyvars free in *either* the rule
	--  *or* the bound variables.  The latter is important.  Consider
	--	ss (x,(y,z)) = (x,z)
	--	RULE:  forall v. fst (ss v) = fst v
	-- The type of the rhs of the rule is just a, but v::(a,(b,c))
	--
	-- We also need to get the completely-uconstrained tyvars of
	-- the LHS, lest they otherwise get defaulted to Any; but we do that
	-- during zonking (see TcHsSyn.zonkRule)

       ; let tpl_ids    = lhs_evs ++ id_bndrs
             forall_tvs = tyVarsOfTypes (rule_ty : map idType tpl_ids)
       ; gbls  <- tcGetGlobalTyVars   -- Even though top level, there might be top-level
                                      -- monomorphic bindings from the MR; test tc111
       ; qtkvs <- quantifyTyVars gbls forall_tvs
       ; traceTc "tcRule" (vcat [ doubleQuotes (ftext name)
                                , ppr forall_tvs
                                , ppr qtkvs
                                , ppr rule_ty
                                , vcat [ ppr id <+> dcolon <+> ppr (idType id) | id <- tpl_ids ] 
                  ])

           -- Simplify the RHS constraints
       ; lcl_env <- getLclEnv
       ; rhs_binds_var <- newTcEvBinds
       ; emitImplication $ Implic { ic_untch  = noUntouchables
                                  , ic_skols  = qtkvs
                                  , ic_fsks   = []
                                  , ic_no_eqs = False
                                  , ic_given  = lhs_evs
                                  , ic_wanted = rhs_wanted
                                  , ic_insol  = insolubleWC rhs_wanted
                                  , ic_binds  = rhs_binds_var
                                  , ic_info   = RuleSkol name
                                  , ic_env    = lcl_env } 

           -- For the LHS constraints we must solve the remaining constraints
           -- (a) so that we report insoluble ones
           -- (b) so that we bind any soluble ones
       ; lhs_binds_var <- newTcEvBinds
       ; emitImplication $ Implic { ic_untch  = noUntouchables
                                  , ic_skols  = qtkvs
                                  , ic_fsks   = []
                                  , ic_no_eqs = False
                                  , ic_given  = lhs_evs
                                  , ic_wanted = other_lhs_wanted
                                  , ic_insol  = insolubleWC other_lhs_wanted
                                  , ic_binds  = lhs_binds_var
                                  , ic_info   = RuleSkol name
                                  , ic_env    = lcl_env } 

       ; return (HsRule name act
		    (map (RuleBndr . noLoc) (qtkvs ++ tpl_ids))
		    (mkHsDictLet (TcEvBinds lhs_binds_var) lhs') fv_lhs
		    (mkHsDictLet (TcEvBinds rhs_binds_var) rhs') fv_rhs) }

tcRuleBndrs :: [RuleBndr Name] -> TcM [Var]
tcRuleBndrs [] 
  = return []
tcRuleBndrs (RuleBndr (L _ name) : rule_bndrs)
  = do 	{ ty <- newFlexiTyVarTy openTypeKind
        ; vars <- tcRuleBndrs rule_bndrs
	; return (mkLocalId name ty : vars) }
tcRuleBndrs (RuleBndrSig (L _ name) rn_ty : rule_bndrs)
--  e.g 	x :: a->a
--  The tyvar 'a' is brought into scope first, just as if you'd written
--		a::*, x :: a->a
  = do	{ let ctxt = RuleSigCtxt name
	; (id_ty, tv_prs) <- tcHsPatSigType ctxt rn_ty
        ; let id  = mkLocalId name id_ty
              tvs = map snd tv_prs   
                    -- tcHsPatSigType returns (Name,TyVar) pairs
                    -- for for RuleSigCtxt their Names are not
                    -- cloned, so we get (n, tv-with-name-n) pairs
                    -- See Note [Pattern signature binders] in TcHsType

	      -- The type variables scope over subsequent bindings; yuk
        ; vars <- tcExtendTyVarEnv tvs $ 
                  tcRuleBndrs rule_bndrs 
	; return (tvs ++ id : vars) }

ruleCtxt :: FastString -> SDoc
ruleCtxt name = ptext (sLit "When checking the transformation rule") <+> 
		doubleQuotes (ftext name)
\end{code}

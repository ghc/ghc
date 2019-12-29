{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[CoreRules]{Transformation rules}
-}

{-# LANGUAGE CPP #-}

-- | Functions for collecting together and applying rewrite rules to a module.
-- The 'CoreRule' datatype itself is declared elsewhere.
module Rules (
        -- ** Constructing
        emptyRuleBase, mkRuleBase, extendRuleBaseList,
        unionRuleBase, pprRuleBase,

        -- ** Checking rule applications
        ruleCheckProgram,

        -- ** Manipulating 'RuleInfo' rules
        mkRuleInfo, extendRuleInfo, addRuleInfo,
        addIdSpecialisations,

        -- * Misc. CoreRule helpers
        rulesOfBinds, getRules, pprRulesForUser,

        lookupRule, mkRule, roughTopNames
    ) where

#include "HsVersions.h"

import GhcPrelude

import CoreSyn          -- All of it
import Module           ( Module, ModuleSet, elemModuleSet )
import CoreSubst
import CoreOpt          ( exprIsLambda_maybe )
import CoreFVs          ( exprFreeVars, exprsFreeVars, bindFreeVars
                        , rulesFreeVarsDSet, exprsOrphNames, exprFreeVarsList )
import CoreUtils        ( exprType, eqExpr, mkTick, mkTicks,
                          stripTicksTopT, stripTicksTopE,
                          isJoinBind )
import PprCore          ( pprRules )
import Type             ( Type, TCvSubst, extendTvSubst, extendCvSubst
                        , mkEmptyTCvSubst, substTy )
import TcType           ( tcSplitTyConApp_maybe )
import TysWiredIn       ( anyTypeOfKind )
import Coercion
import CoreTidy         ( tidyRules )
import Id
import IdInfo           ( RuleInfo( RuleInfo ) )
import Var
import VarEnv
import VarSet
import Name             ( Name, NamedThing(..), nameIsLocalOrFrom )
import NameSet
import NameEnv
import UniqFM
import Unify            ( ruleMatchTyKiX )
import BasicTypes
import DynFlags         ( DynFlags )
import Outputable
import FastString
import Maybes
import Bag
import Util
import Data.List
import Data.Ord
import Control.Monad    ( guard )

{-
Note [Overall plumbing for rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* After the desugarer:
   - The ModGuts initially contains mg_rules :: [CoreRule] of
     locally-declared rules for imported Ids.
   - Locally-declared rules for locally-declared Ids are attached to
     the IdInfo for that Id.  See Note [Attach rules to local ids] in
     DsBinds

* TidyPgm strips off all the rules from local Ids and adds them to
  mg_rules, so that the ModGuts has *all* the locally-declared rules.

* The HomePackageTable contains a ModDetails for each home package
  module.  Each contains md_rules :: [CoreRule] of rules declared in
  that module.  The HomePackageTable grows as ghc --make does its
  up-sweep.  In batch mode (ghc -c), the HPT is empty; all imported modules
  are treated by the "external" route, discussed next, regardless of
  which package they come from.

* The ExternalPackageState has a single eps_rule_base :: RuleBase for
  Ids in other packages.  This RuleBase simply grow monotonically, as
  ghc --make compiles one module after another.

  During simplification, interface files may get demand-loaded,
  as the simplifier explores the unfoldings for Ids it has in
  its hand.  (Via an unsafePerformIO; the EPS is really a cache.)
  That in turn may make the EPS rule-base grow.  In contrast, the
  HPT never grows in this way.

* The result of all this is that during Core-to-Core optimisation
  there are four sources of rules:

    (a) Rules in the IdInfo of the Id they are a rule for.  These are
        easy: fast to look up, and if you apply a substitution then
        it'll be applied to the IdInfo as a matter of course.

    (b) Rules declared in this module for imported Ids, kept in the
        ModGuts. If you do a substitution, you'd better apply the
        substitution to these.  There are seldom many of these.

    (c) Rules declared in the HomePackageTable.  These never change.

    (d) Rules in the ExternalPackageTable. These can grow in response
        to lazy demand-loading of interfaces.

* At the moment (c) is carried in a reader-monad way by the CoreMonad.
  The HomePackageTable doesn't have a single RuleBase because technically
  we should only be able to "see" rules "below" this module; so we
  generate a RuleBase for (c) by combing rules from all the modules
  "below" us.  That's why we can't just select the home-package RuleBase
  from HscEnv.

  [NB: we are inconsistent here.  We should do the same for external
  packages, but we don't.  Same for type-class instances.]

* So in the outer simplifier loop, we combine (b-d) into a single
  RuleBase, reading
     (b) from the ModGuts,
     (c) from the CoreMonad, and
     (d) from its mutable variable
  [Of coures this means that we won't see new EPS rules that come in
  during a single simplifier iteration, but that probably does not
  matter.]


************************************************************************
*                                                                      *
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
*                                                                      *
************************************************************************

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
-}

mkRule :: Module -> Bool -> Bool -> RuleName -> Activation
       -> Name -> [CoreBndr] -> [CoreExpr] -> CoreExpr -> CoreRule
-- ^ Used to make 'CoreRule' for an 'Id' defined in the module being
-- compiled. See also 'CoreSyn.CoreRule'
mkRule this_mod is_auto is_local name act fn bndrs args rhs
  = Rule { ru_name = name, ru_fn = fn, ru_act = act,
           ru_bndrs = bndrs, ru_args = args,
           ru_rhs = rhs,
           ru_rough = roughTopNames args,
           ru_origin = this_mod,
           ru_orphan = orph,
           ru_auto = is_auto, ru_local = is_local }
  where
        -- Compute orphanhood.  See Note [Orphans] in InstEnv
        -- A rule is an orphan only if none of the variables
        -- mentioned on its left-hand side are locally defined
    lhs_names = extendNameSet (exprsOrphNames args) fn

        -- Since rules get eventually attached to one of the free names
        -- from the definition when compiling the ABI hash, we should make
        -- it deterministic. This chooses the one with minimal OccName
        -- as opposed to uniq value.
    local_lhs_names = filterNameSet (nameIsLocalOrFrom this_mod) lhs_names
    orph = chooseOrphanAnchor local_lhs_names

--------------
roughTopNames :: [CoreExpr] -> [Maybe Name]
-- ^ Find the \"top\" free names of several expressions.
-- Such names are either:
--
-- 1. The function finally being applied to in an application chain
--    (if that name is a GlobalId: see "Var#globalvslocal"), or
--
-- 2. The 'TyCon' if the expression is a 'Type'
--
-- This is used for the fast-match-check for rules;
--      if the top names don't match, the rest can't
roughTopNames args = map roughTopName args

roughTopName :: CoreExpr -> Maybe Name
roughTopName (Type ty) = case tcSplitTyConApp_maybe ty of
                               Just (tc,_) -> Just (getName tc)
                               Nothing     -> Nothing
roughTopName (Coercion _) = Nothing
roughTopName (App f _) = roughTopName f
roughTopName (Var f)   | isGlobalId f   -- Note [Care with roughTopName]
                       , isDataConWorkId f || idArity f > 0
                       = Just (idName f)
roughTopName (Tick t e) | tickishFloatable t
                        = roughTopName e
roughTopName _ = Nothing

ruleCantMatch :: [Maybe Name] -> [Maybe Name] -> Bool
-- ^ @ruleCantMatch tpl actual@ returns True only if @actual@
-- definitely can't match @tpl@ by instantiating @tpl@.
-- It's only a one-way match; unlike instance matching we
-- don't consider unification.
--
-- Notice that [_$_]
--      @ruleCantMatch [Nothing] [Just n2] = False@
--      Reason: a template variable can be instantiated by a constant
-- Also:
--      @ruleCantMatch [Just n1] [Nothing] = False@
--      Reason: a local variable @v@ in the actuals might [_$_]

ruleCantMatch (Just n1 : ts) (Just n2 : as) = n1 /= n2 || ruleCantMatch ts as
ruleCantMatch (_       : ts) (_       : as) = ruleCantMatch ts as
ruleCantMatch _              _              = False

{-
Note [Care with roughTopName]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
    module M where { x = a:b }
    module N where { ...f x...
                     RULE f (p:q) = ... }
You'd expect the rule to match, because the matcher can
look through the unfolding of 'x'.  So we must avoid roughTopName
returning 'M.x' for the call (f x), or else it'll say "can't match"
and we won't even try!!

However, suppose we have
         RULE g (M.h x) = ...
         foo = ...(g (M.k v))....
where k is a *function* exported by M.  We never really match
functions (lambdas) except by name, so in this case it seems like
a good idea to treat 'M.k' as a roughTopName of the call.
-}

pprRulesForUser :: DynFlags -> [CoreRule] -> SDoc
-- (a) tidy the rules
-- (b) sort them into order based on the rule name
-- (c) suppress uniques (unless -dppr-debug is on)
-- This combination makes the output stable so we can use in testing
-- It's here rather than in PprCore because it calls tidyRules
pprRulesForUser dflags rules
  = withPprStyle (defaultUserStyle dflags) $
    pprRules $
    sortBy (comparing ruleName) $
    tidyRules emptyTidyEnv rules

{-
************************************************************************
*                                                                      *
                RuleInfo: the rules in an IdInfo
*                                                                      *
************************************************************************
-}

-- | Make a 'RuleInfo' containing a number of 'CoreRule's, suitable
-- for putting into an 'IdInfo'
mkRuleInfo :: [CoreRule] -> RuleInfo
mkRuleInfo rules = RuleInfo rules (rulesFreeVarsDSet rules)

extendRuleInfo :: RuleInfo -> [CoreRule] -> RuleInfo
extendRuleInfo (RuleInfo rs1 fvs1) rs2
  = RuleInfo (rs2 ++ rs1) (rulesFreeVarsDSet rs2 `unionDVarSet` fvs1)

addRuleInfo :: RuleInfo -> RuleInfo -> RuleInfo
addRuleInfo (RuleInfo rs1 fvs1) (RuleInfo rs2 fvs2)
  = RuleInfo (rs1 ++ rs2) (fvs1 `unionDVarSet` fvs2)

addIdSpecialisations :: Id -> [CoreRule] -> Id
addIdSpecialisations id rules
  | null rules
  = id
  | otherwise
  = setIdSpecialisation id $
    extendRuleInfo (idSpecialisation id) rules

-- | Gather all the rules for locally bound identifiers from the supplied bindings
rulesOfBinds :: [CoreBind] -> [CoreRule]
rulesOfBinds binds = concatMap (concatMap idCoreRules . bindersOf) binds

getRules :: RuleEnv -> Id -> [CoreRule]
-- See Note [Where rules are found]
getRules (RuleEnv { re_base = rule_base, re_visible_orphs = orphs }) fn
  = idCoreRules fn ++ filter (ruleIsVisible orphs) imp_rules
  where
    imp_rules = lookupNameEnv rule_base (idName fn) `orElse` []

ruleIsVisible :: ModuleSet -> CoreRule -> Bool
ruleIsVisible _ BuiltinRule{} = True
ruleIsVisible vis_orphs Rule { ru_orphan = orph, ru_origin = origin }
    = notOrphan orph || origin `elemModuleSet` vis_orphs

{- Note [Where rules are found]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rules for an Id come from two places:
  (a) the ones it is born with, stored inside the Id iself (idCoreRules fn),
  (b) rules added in other modules, stored in the global RuleBase (imp_rules)

It's tempting to think that
     - LocalIds have only (a)
     - non-LocalIds have only (b)

but that isn't quite right:

     - PrimOps and ClassOps are born with a bunch of rules inside the Id,
       even when they are imported

     - The rules in PrelRules.builtinRules should be active even
       in the module defining the Id (when it's a LocalId), but
       the rules are kept in the global RuleBase


************************************************************************
*                                                                      *
                RuleBase
*                                                                      *
************************************************************************
-}

-- RuleBase itself is defined in CoreSyn, along with CoreRule

emptyRuleBase :: RuleBase
emptyRuleBase = emptyNameEnv

mkRuleBase :: [CoreRule] -> RuleBase
mkRuleBase rules = extendRuleBaseList emptyRuleBase rules

extendRuleBaseList :: RuleBase -> [CoreRule] -> RuleBase
extendRuleBaseList rule_base new_guys
  = foldl' extendRuleBase rule_base new_guys

unionRuleBase :: RuleBase -> RuleBase -> RuleBase
unionRuleBase rb1 rb2 = plusNameEnv_C (++) rb1 rb2

extendRuleBase :: RuleBase -> CoreRule -> RuleBase
extendRuleBase rule_base rule
  = extendNameEnv_Acc (:) singleton rule_base (ruleIdName rule) rule

pprRuleBase :: RuleBase -> SDoc
pprRuleBase rules = pprUFM rules $ \rss ->
  vcat [ pprRules (tidyRules emptyTidyEnv rs)
       | rs <- rss ]

{-
************************************************************************
*                                                                      *
                        Matching
*                                                                      *
************************************************************************
-}

-- | The main rule matching function. Attempts to apply all (active)
-- supplied rules to this instance of an application in a given
-- context, returning the rule applied and the resulting expression if
-- successful.
lookupRule :: DynFlags -> InScopeEnv
           -> (Activation -> Bool)      -- When rule is active
           -> Id -> [CoreExpr]
           -> [CoreRule] -> Maybe (CoreRule, CoreExpr)

-- See Note [Extra args in rule matching]
-- See comments on matchRule
lookupRule dflags in_scope is_active fn args rules
  = -- pprTrace "matchRules" (ppr fn <+> ppr args $$ ppr rules ) $
    case go [] rules of
        []     -> Nothing
        (m:ms) -> Just (findBest (fn,args') m ms)
  where
    rough_args = map roughTopName args

    -- Strip ticks from arguments, see note [Tick annotations in RULE
    -- matching]. We only collect ticks if a rule actually matches -
    -- this matters for performance tests.
    args' = map (stripTicksTopE tickishFloatable) args
    ticks = concatMap (stripTicksTopT tickishFloatable) args

    go :: [(CoreRule,CoreExpr)] -> [CoreRule] -> [(CoreRule,CoreExpr)]
    go ms [] = ms
    go ms (r:rs)
      | Just e <- matchRule dflags in_scope is_active fn args' rough_args r
      = go ((r,mkTicks ticks e):ms) rs
      | otherwise
      = -- pprTrace "match failed" (ppr r $$ ppr args $$
        --   ppr [ (arg_id, unfoldingTemplate unf)
        --       | Var arg_id <- args
        --       , let unf = idUnfolding arg_id
        --       , isCheapUnfolding unf] )
        go ms rs

findBest :: (Id, [CoreExpr])
         -> (CoreRule,CoreExpr) -> [(CoreRule,CoreExpr)] -> (CoreRule,CoreExpr)
-- All these pairs matched the expression
-- Return the pair the most specific rule
-- The (fn,args) is just for overlap reporting

findBest _      (rule,ans)   [] = (rule,ans)
findBest target (rule1,ans1) ((rule2,ans2):prs)
  | rule1 `isMoreSpecific` rule2 = findBest target (rule1,ans1) prs
  | rule2 `isMoreSpecific` rule1 = findBest target (rule2,ans2) prs
  | debugIsOn = let pp_rule rule
                      = ifPprDebug (ppr rule)
                                   (doubleQuotes (ftext (ruleName rule)))
                in pprTrace "Rules.findBest: rule overlap (Rule 1 wins)"
                         (vcat [ whenPprDebug $
                                 text "Expression to match:" <+> ppr fn
                                 <+> sep (map ppr args)
                               , text "Rule 1:" <+> pp_rule rule1
                               , text "Rule 2:" <+> pp_rule rule2]) $
                findBest target (rule1,ans1) prs
  | otherwise = findBest target (rule1,ans1) prs
  where
    (fn,args) = target

isMoreSpecific :: CoreRule -> CoreRule -> Bool
-- This tests if one rule is more specific than another
-- We take the view that a BuiltinRule is less specific than
-- anything else, because we want user-define rules to "win"
-- In particular, class ops have a built-in rule, but we
-- any user-specific rules to win
--   eg (#4397)
--      truncate :: (RealFrac a, Integral b) => a -> b
--      {-# RULES "truncate/Double->Int" truncate = double2Int #-}
--      double2Int :: Double -> Int
--   We want the specific RULE to beat the built-in class-op rule
isMoreSpecific (BuiltinRule {}) _                = False
isMoreSpecific (Rule {})        (BuiltinRule {}) = True
isMoreSpecific (Rule { ru_bndrs = bndrs1, ru_args = args1 })
               (Rule { ru_bndrs = bndrs2, ru_args = args2
                     , ru_name = rule_name2, ru_rhs = rhs })
  = isJust (matchN (in_scope, id_unfolding_fun) rule_name2 bndrs2 args2 args1 rhs)
  where
   id_unfolding_fun _ = NoUnfolding     -- Don't expand in templates
   in_scope = mkInScopeSet (mkVarSet bndrs1)
        -- Actually we should probably include the free vars
        -- of rule1's args, but I can't be bothered

noBlackList :: Activation -> Bool
noBlackList _ = False           -- Nothing is black listed

{-
Note [Extra args in rule matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we find a matching rule, we return (Just (rule, rhs)),
but the rule firing has only consumed as many of the input args
as the ruleArity says.  It's up to the caller to keep track
of any left-over args.  E.g. if you call
        lookupRule ... f [e1, e2, e3]
and it returns Just (r, rhs), where r has ruleArity 2
then the real rewrite is
        f e1 e2 e3 ==> rhs e3

You might think it'd be cleaner for lookupRule to deal with the
leftover arguments, by applying 'rhs' to them, but the main call
in the Simplifier works better as it is.  Reason: the 'args' passed
to lookupRule are the result of a lazy substitution
-}

------------------------------------
matchRule :: DynFlags -> InScopeEnv -> (Activation -> Bool)
          -> Id -> [CoreExpr] -> [Maybe Name]
          -> CoreRule -> Maybe CoreExpr

-- If (matchRule rule args) returns Just (name,rhs)
-- then (f args) matches the rule, and the corresponding
-- rewritten RHS is rhs
--
-- The returned expression is occurrence-analysed
--
--      Example
--
-- The rule
--      forall f g x. map f (map g x) ==> map (f . g) x
-- is stored
--      CoreRule "map/map"
--               [f,g,x]                -- tpl_vars
--               [f,map g x]            -- tpl_args
--               map (f.g) x)           -- rhs
--
-- Then the call: matchRule the_rule [e1,map e2 e3]
--        = Just ("map/map", (\f,g,x -> rhs) e1 e2 e3)
--
-- Any 'surplus' arguments in the input are simply put on the end
-- of the output.

matchRule dflags rule_env _is_active fn args _rough_args
          (BuiltinRule { ru_try = match_fn })
-- Built-in rules can't be switched off, it seems
  = case match_fn dflags rule_env fn args of
        Nothing   -> Nothing
        Just expr -> Just expr

matchRule _ in_scope is_active _ args rough_args
          (Rule { ru_name = rule_name, ru_act = act, ru_rough = tpl_tops
                , ru_bndrs = tpl_vars, ru_args = tpl_args, ru_rhs = rhs })
  | not (is_active act)               = Nothing
  | ruleCantMatch tpl_tops rough_args = Nothing
  | otherwise = matchN in_scope rule_name tpl_vars tpl_args args rhs

---------------------------------------
matchN  :: InScopeEnv
        -> RuleName -> [Var] -> [CoreExpr]
        -> [CoreExpr] -> CoreExpr           -- ^ Target; can have more elements than the template
        -> Maybe CoreExpr
-- For a given match template and context, find bindings to wrap around
-- the entire result and what should be substituted for each template variable.
-- Fail if there are two few actual arguments from the target to match the template

matchN (in_scope, id_unf) rule_name tmpl_vars tmpl_es target_es rhs
  = do  { rule_subst <- go init_menv emptyRuleSubst tmpl_es target_es
        ; let (_, matched_es) = mapAccumL (lookup_tmpl rule_subst)
                                          (mkEmptyTCvSubst in_scope) $
                                tmpl_vars `zip` tmpl_vars1
              bind_wrapper = rs_binds rule_subst
                             -- Floated bindings; see Note [Matching lets]
       ; return (bind_wrapper $
                 mkLams tmpl_vars rhs `mkApps` matched_es) }
  where
    (init_rn_env, tmpl_vars1) = mapAccumL rnBndrL (mkRnEnv2 in_scope) tmpl_vars
                  -- See Note [Cloning the template binders]

    init_menv = RV { rv_tmpls = mkVarSet tmpl_vars1
                   , rv_lcl   = init_rn_env
                   , rv_fltR  = mkEmptySubst (rnInScopeSet init_rn_env)
                   , rv_unf   = id_unf }

    go _    subst []     _      = Just subst
    go _    _     _      []     = Nothing       -- Fail if too few actual args
    go menv subst (t:ts) (e:es) = do { subst1 <- match menv subst t e
                                     ; go menv subst1 ts es }

    lookup_tmpl :: RuleSubst -> TCvSubst -> (InVar,OutVar) -> (TCvSubst, CoreExpr)
                   -- Need to return a RuleSubst solely for the benefit of mk_fake_ty
    lookup_tmpl (RS { rs_tv_subst = tv_subst, rs_id_subst = id_subst })
                tcv_subst (tmpl_var, tmpl_var1)
        | isId tmpl_var1
        = case lookupVarEnv id_subst tmpl_var1 of
            Just e | Coercion co <- e
                   -> (Type.extendCvSubst tcv_subst tmpl_var1 co, Coercion co)
                   | otherwise
                   -> (tcv_subst, e)
            Nothing | Just refl_co <- isReflCoVar_maybe tmpl_var1
                    , let co = Coercion.substCo tcv_subst refl_co
                    -> -- See Note [Unbound RULE binders]
                       (Type.extendCvSubst tcv_subst tmpl_var1 co, Coercion co)
                    | otherwise
                    -> unbound tmpl_var

        | otherwise
        = (Type.extendTvSubst tcv_subst tmpl_var1 ty', Type ty')
        where
          ty' = case lookupVarEnv tv_subst tmpl_var1 of
                  Just ty -> ty
                  Nothing -> fake_ty   -- See Note [Unbound RULE binders]
          fake_ty = anyTypeOfKind (Type.substTy tcv_subst (tyVarKind tmpl_var1))
                    -- This substitution is the sole reason we accumulate
                    -- TCvSubst in lookup_tmpl

    unbound tmpl_var
       = pprPanic "Template variable unbound in rewrite rule" $
         vcat [ text "Variable:" <+> ppr tmpl_var <+> dcolon <+> ppr (varType tmpl_var)
              , text "Rule" <+> pprRuleName rule_name
              , text "Rule bndrs:" <+> ppr tmpl_vars
              , text "LHS args:" <+> ppr tmpl_es
              , text "Actual args:" <+> ppr target_es ]


{- Note [Unbound RULE binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be the case that the binder in a rule is not actually
bound on the LHS:

* Type variables.  Type synonyms with phantom args can give rise to
  unbound template type variables.  Consider this (#10689,
  simplCore/should_compile/T10689):

    type Foo a b = b

    f :: Eq a => a -> Bool
    f x = x==x

    {-# RULES "foo" forall (x :: Foo a Char). f x = True #-}
    finkle = f 'c'

  The rule looks like
    forall (a::*) (d::Eq Char) (x :: Foo a Char).
         f (Foo a Char) d x = True

  Matching the rule won't bind 'a', and legitimately so.  We fudge by
  pretending that 'a' is bound to (Any :: *).

* Coercion variables.  On the LHS of a RULE for a local binder
  we might have
    RULE forall (c :: a~b). f (x |> c) = e
  Now, if that binding is inlined, so that a=b=Int, we'd get
    RULE forall (c :: Int~Int). f (x |> c) = e
  and now when we simplify the LHS (Simplify.simplRule) we
  optCoercion (look at the CoVarCo case) will turn that 'c' into Refl:
    RULE forall (c :: Int~Int). f (x |> <Int>) = e
  and then perhaps drop it altogether.  Now 'c' is unbound.

  It's tricky to be sure this never happens, so instead I
  say it's OK to have an unbound coercion binder in a RULE
  provided its type is (c :: t~t).  Then, when the RULE
  fires we can substitute <t> for c.

  This actually happened (in a RULE for a local function)
  in #13410, and also in test T10602.

Note [Cloning the template binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following match (example 1):
        Template:  forall x.  f x
        Target:               f (x+1)
This should succeed, because the template variable 'x' has nothing to
do with the 'x' in the target.

Likewise this one (example 2):
        Template:  forall x. f (\x.x)
        Target:              f (\y.y)

We achieve this simply by using rnBndrL to clone the template
binders if they are already in scope.

------ Historical note -------
At one point I tried simply adding the template binders to the
in-scope set /without/ cloning them, but that failed in a horribly
obscure way in #14777.  Problem was that during matching we look
up target-term variables in the in-scope set (see Note [Lookup
in-scope]).  If a target-term variable happens to name-clash with a
template variable, that lookup will find the template variable, which
is /utterly/ bogus.  In #14777, this transformed a term variable
into a type variable, and then crashed when we wanted its idInfo.
------ End of historical note -------


************************************************************************
*                                                                      *
                   The main matcher
*                                                                      *
********************************************************************* -}

-- * The domain of the TvSubstEnv and IdSubstEnv are the template
--   variables passed into the match.
--
-- * The BindWrapper in a RuleSubst are the bindings floated out
--   from nested matches; see the Let case of match, below
--
data RuleMatchEnv
  = RV { rv_lcl   :: RnEnv2          -- Renamings for *local bindings*
                                     --   (lambda/case)
       , rv_tmpls :: VarSet          -- Template variables
                                     --   (after applying envL of rv_lcl)
       , rv_fltR  :: Subst           -- Renamings for floated let-bindings
                                     --   (domain disjoint from envR of rv_lcl)
                                     -- See Note [Matching lets]
       , rv_unf :: IdUnfoldingFun
       }

rvInScopeEnv :: RuleMatchEnv -> InScopeEnv
rvInScopeEnv renv = (rnInScopeSet (rv_lcl renv), rv_unf renv)

data RuleSubst = RS { rs_tv_subst :: TvSubstEnv   -- Range is the
                    , rs_id_subst :: IdSubstEnv   --   template variables
                    , rs_binds    :: BindWrapper  -- Floated bindings
                    , rs_bndrs    :: VarSet       -- Variables bound by floated lets
                    }

type BindWrapper = CoreExpr -> CoreExpr
  -- See Notes [Matching lets] and [Matching cases]
  -- we represent the floated bindings as a core-to-core function

emptyRuleSubst :: RuleSubst
emptyRuleSubst = RS { rs_tv_subst = emptyVarEnv, rs_id_subst = emptyVarEnv
                    , rs_binds = \e -> e, rs_bndrs = emptyVarSet }

--      At one stage I tried to match even if there are more
--      template args than real args.

--      I now think this is probably a bad idea.
--      Should the template (map f xs) match (map g)?  I think not.
--      For a start, in general eta expansion wastes work.
--      SLPJ July 99

match :: RuleMatchEnv
      -> RuleSubst
      -> CoreExpr               -- Template
      -> CoreExpr               -- Target
      -> Maybe RuleSubst

-- We look through certain ticks. See note [Tick annotations in RULE matching]
match renv subst e1 (Tick t e2)
  | tickishFloatable t
  = match renv subst' e1 e2
  where subst' = subst { rs_binds = rs_binds subst . mkTick t }
match _ _ e@Tick{} _
  = pprPanic "Tick in rule" (ppr e)

-- See the notes with Unify.match, which matches types
-- Everything is very similar for terms

-- Interesting examples:
-- Consider matching
--      \x->f      against    \f->f
-- When we meet the lambdas we must remember to rename f to f' in the
-- second expression.  The RnEnv2 does that.
--
-- Consider matching
--      forall a. \b->b    against   \a->3
-- We must rename the \a.  Otherwise when we meet the lambdas we
-- might substitute [a/b] in the template, and then erroneously
-- succeed in matching what looks like the template variable 'a' against 3.

-- The Var case follows closely what happens in Unify.match
match renv subst (Var v1) e2
  = match_var renv subst v1 e2

match renv subst e1 (Var v2)      -- Note [Expanding variables]
  | not (inRnEnvR rn_env v2) -- Note [Do not expand locally-bound variables]
  , Just e2' <- expandUnfolding_maybe (rv_unf renv v2')
  = match (renv { rv_lcl = nukeRnEnvR rn_env }) subst e1 e2'
  where
    v2'    = lookupRnInScope rn_env v2
    rn_env = rv_lcl renv
        -- Notice that we look up v2 in the in-scope set
        -- See Note [Lookup in-scope]
        -- No need to apply any renaming first (hence no rnOccR)
        -- because of the not-inRnEnvR

match renv subst e1 (Let bind e2)
  | -- pprTrace "match:Let" (vcat [ppr bind, ppr $ okToFloat (rv_lcl renv) (bindFreeVars bind)]) $
    not (isJoinBind bind) -- can't float join point out of argument position
  , okToFloat (rv_lcl renv) (bindFreeVars bind) -- See Note [Matching lets]
  = match (renv { rv_fltR = flt_subst' })
          (subst { rs_binds = rs_binds subst . Let bind'
                 , rs_bndrs = extendVarSetList (rs_bndrs subst) new_bndrs })
          e1 e2
  where
    flt_subst = addInScopeSet (rv_fltR renv) (rs_bndrs subst)
    (flt_subst', bind') = substBind flt_subst bind
    new_bndrs = bindersOf bind'

{- Disabled: see Note [Matching cases] below
match renv (tv_subst, id_subst, binds) e1
      (Case scrut case_bndr ty [(con, alt_bndrs, rhs)])
  | exprOkForSpeculation scrut  -- See Note [Matching cases]
  , okToFloat rn_env bndrs (exprFreeVars scrut)
  = match (renv { me_env = rn_env' })
          (tv_subst, id_subst, binds . case_wrap)
          e1 rhs
  where
    rn_env   = me_env renv
    rn_env'  = extendRnInScopeList rn_env bndrs
    bndrs    = case_bndr : alt_bndrs
    case_wrap rhs' = Case scrut case_bndr ty [(con, alt_bndrs, rhs')]
-}

match _ subst (Lit lit1) (Lit lit2)
  | lit1 == lit2
  = Just subst

match renv subst (App f1 a1) (App f2 a2)
  = do  { subst' <- match renv subst f1 f2
        ; match renv subst' a1 a2 }

match renv subst (Lam x1 e1) e2
  | Just (x2, e2, ts) <- exprIsLambda_maybe (rvInScopeEnv renv) e2
  = let renv' = renv { rv_lcl = rnBndr2 (rv_lcl renv) x1 x2
                     , rv_fltR = delBndr (rv_fltR renv) x2 }
        subst' = subst { rs_binds = rs_binds subst . flip (foldr mkTick) ts }
    in  match renv' subst' e1 e2

match renv subst (Case e1 x1 ty1 alts1) (Case e2 x2 ty2 alts2)
  = do  { subst1 <- match_ty renv subst ty1 ty2
        ; subst2 <- match renv subst1 e1 e2
        ; let renv' = rnMatchBndr2 renv subst x1 x2
        ; match_alts renv' subst2 alts1 alts2   -- Alts are both sorted
        }

match renv subst (Type ty1) (Type ty2)
  = match_ty renv subst ty1 ty2
match renv subst (Coercion co1) (Coercion co2)
  = match_co renv subst co1 co2

match renv subst (Cast e1 co1) (Cast e2 co2)
  = do  { subst1 <- match_co renv subst co1 co2
        ; match renv subst1 e1 e2 }

-- Everything else fails
match _ _ _e1 _e2 = -- pprTrace "Failing at" ((text "e1:" <+> ppr _e1) $$ (text "e2:" <+> ppr _e2)) $
                    Nothing

-------------
match_co :: RuleMatchEnv
         -> RuleSubst
         -> Coercion
         -> Coercion
         -> Maybe RuleSubst
match_co renv subst co1 co2
  | Just cv <- getCoVar_maybe co1
  = match_var renv subst cv (Coercion co2)
  | Just (ty1, r1) <- isReflCo_maybe co1
  = do { (ty2, r2) <- isReflCo_maybe co2
       ; guard (r1 == r2)
       ; match_ty renv subst ty1 ty2 }
match_co renv subst co1 co2
  | Just (tc1, cos1) <- splitTyConAppCo_maybe co1
  = case splitTyConAppCo_maybe co2 of
      Just (tc2, cos2)
        |  tc1 == tc2
        -> match_cos renv subst cos1 cos2
      _ -> Nothing
match_co renv subst co1 co2
  | Just (arg1, res1) <- splitFunCo_maybe co1
  = case splitFunCo_maybe co2 of
      Just (arg2, res2)
        -> match_cos renv subst [arg1, res1] [arg2, res2]
      _ -> Nothing
match_co _ _ _co1 _co2
    -- Currently just deals with CoVarCo, TyConAppCo and Refl
#if defined(DEBUG)
  = pprTrace "match_co: needs more cases" (ppr _co1 $$ ppr _co2) Nothing
#else
  = Nothing
#endif

match_cos :: RuleMatchEnv
         -> RuleSubst
         -> [Coercion]
         -> [Coercion]
         -> Maybe RuleSubst
match_cos renv subst (co1:cos1) (co2:cos2) =
  do { subst' <- match_co renv subst co1 co2
     ; match_cos renv subst' cos1 cos2 }
match_cos _ subst [] [] = Just subst
match_cos _ _ cos1 cos2 = pprTrace "match_cos: not same length" (ppr cos1 $$ ppr cos2) Nothing

-------------
rnMatchBndr2 :: RuleMatchEnv -> RuleSubst -> Var -> Var -> RuleMatchEnv
rnMatchBndr2 renv subst x1 x2
  = renv { rv_lcl  = rnBndr2 rn_env x1 x2
         , rv_fltR = delBndr (rv_fltR renv) x2 }
  where
    rn_env = addRnInScopeSet (rv_lcl renv) (rs_bndrs subst)
    -- Typically this is a no-op, but it may matter if
    -- there are some floated let-bindings

------------------------------------------
match_alts :: RuleMatchEnv
           -> RuleSubst
           -> [CoreAlt]         -- Template
           -> [CoreAlt]         -- Target
           -> Maybe RuleSubst
match_alts _ subst [] []
  = return subst
match_alts renv subst ((c1,vs1,r1):alts1) ((c2,vs2,r2):alts2)
  | c1 == c2
  = do  { subst1 <- match renv' subst r1 r2
        ; match_alts renv subst1 alts1 alts2 }
  where
    renv' = foldl' mb renv (vs1 `zip` vs2)
    mb renv (v1,v2) = rnMatchBndr2 renv subst v1 v2

match_alts _ _ _ _
  = Nothing

------------------------------------------
okToFloat :: RnEnv2 -> VarSet -> Bool
okToFloat rn_env bind_fvs
  = allVarSet not_captured bind_fvs
  where
    not_captured fv = not (inRnEnvR rn_env fv)

------------------------------------------
match_var :: RuleMatchEnv
          -> RuleSubst
          -> Var                -- Template
          -> CoreExpr        -- Target
          -> Maybe RuleSubst
match_var renv@(RV { rv_tmpls = tmpls, rv_lcl = rn_env, rv_fltR = flt_env })
          subst v1 e2
  | v1' `elemVarSet` tmpls
  = match_tmpl_var renv subst v1' e2

  | otherwise   -- v1' is not a template variable; check for an exact match with e2
  = case e2 of  -- Remember, envR of rn_env is disjoint from rv_fltR
       Var v2 | v1' == rnOccR rn_env v2
              -> Just subst

              | Var v2' <- lookupIdSubst (text "match_var") flt_env v2
              , v1' == v2'
              -> Just subst

       _ -> Nothing

  where
    v1' = rnOccL rn_env v1
        -- If the template is
        --      forall x. f x (\x -> x) = ...
        -- Then the x inside the lambda isn't the
        -- template x, so we must rename first!

------------------------------------------
match_tmpl_var :: RuleMatchEnv
               -> RuleSubst
               -> Var                -- Template
               -> CoreExpr              -- Target
               -> Maybe RuleSubst

match_tmpl_var renv@(RV { rv_lcl = rn_env, rv_fltR = flt_env })
               subst@(RS { rs_id_subst = id_subst, rs_bndrs = let_bndrs })
               v1' e2
  | any (inRnEnvR rn_env) (exprFreeVarsList e2)
  = Nothing     -- Occurs check failure
                -- e.g. match forall a. (\x-> a x) against (\y. y y)

  | Just e1' <- lookupVarEnv id_subst v1'
  = if eqExpr (rnInScopeSet rn_env) e1' e2'
    then Just subst
    else Nothing

  | otherwise
  =             -- Note [Matching variable types]
                -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                -- However, we must match the *types*; e.g.
                --   forall (c::Char->Int) (x::Char).
                --      f (c x) = "RULE FIRED"
                -- We must only match on args that have the right type
                -- It's actually quite difficult to come up with an example that shows
                -- you need type matching, esp since matching is left-to-right, so type
                -- args get matched first.  But it's possible (e.g. simplrun008) and
                -- this is the Right Thing to do
    do { subst' <- match_ty renv subst (idType v1') (exprType e2)
       ; return (subst' { rs_id_subst = id_subst' }) }
  where
    -- e2' is the result of applying flt_env to e2
    e2' | isEmptyVarSet let_bndrs = e2
        | otherwise = substExpr (text "match_tmpl_var") flt_env e2

    id_subst' = extendVarEnv (rs_id_subst subst) v1' e2'
         -- No further renaming to do on e2',
         -- because no free var of e2' is in the rnEnvR of the envt

------------------------------------------
match_ty :: RuleMatchEnv
         -> RuleSubst
         -> Type                -- Template
         -> Type                -- Target
         -> Maybe RuleSubst
-- Matching Core types: use the matcher in TcType.
-- Notice that we treat newtypes as opaque.  For example, suppose
-- we have a specialised version of a function at a newtype, say
--      newtype T = MkT Int
-- We only want to replace (f T) with f', not (f Int).

match_ty renv subst ty1 ty2
  = do  { tv_subst'
            <- Unify.ruleMatchTyKiX (rv_tmpls renv) (rv_lcl renv) tv_subst ty1 ty2
        ; return (subst { rs_tv_subst = tv_subst' }) }
  where
    tv_subst = rs_tv_subst subst

{-
Note [Expanding variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is another Very Important rule: if the term being matched is a
variable, we expand it so long as its unfolding is "expandable". (Its
occurrence information is not necessarily up to date, so we don't use
it.)  By "expandable" we mean a WHNF or a "constructor-like" application.
This is the key reason for "constructor-like" Ids.  If we have
     {-# NOINLINE [1] CONLIKE g #-}
     {-# RULE f (g x) = h x #-}
then in the term
   let v = g 3 in ....(f v)....
we want to make the rule fire, to replace (f v) with (h 3).

Note [Do not expand locally-bound variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do *not* expand locally-bound variables, else there's a worry that the
unfolding might mention variables that are themselves renamed.
Example
          case x of y { (p,q) -> ...y... }
Don't expand 'y' to (p,q) because p,q might themselves have been
renamed.  Essentially we only expand unfoldings that are "outside"
the entire match.

Hence, (a) the guard (not (isLocallyBoundR v2))
       (b) when we expand we nuke the renaming envt (nukeRnEnvR).

Note [Tick annotations in RULE matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We used to unconditionally look through Notes in both template and
expression being matched. This is actually illegal for counting or
cost-centre-scoped ticks, because we have no place to put them without
changing entry counts and/or costs. So now we just fail the match in
these cases.

On the other hand, where we are allowed to insert new cost into the
tick scope, we can float them upwards to the rule application site.

cf Note [Notes in call patterns] in SpecConstr

Note [Rule templates are devoid of ticks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As mentioned in Note [Tick annotations in RULE matching], the matcher allows
ticks only in very few cases. In particular, they are completely disallowed
in the rule template. Core Lint checks this invariant.

However, there are a few ways in which ticks can sneak in to the template.
#17619 is one particularly tricky way:

 1. SpecConstr creates a rule on `f` with a free variable `x` in its template.
 2. CSE rewrites the RHS of `x` to `<tick> y`
 3. The simplifier unconditionally post-inlines `x`
 4. The simplifier simplifes `f`'s rules and, in so doing, substitutes `x ~>
    <tick> y`, introducing a tick into the template of the rule.

To avoid this, we strip ticks after substituting in to rule templates.

Note [Matching lets]
~~~~~~~~~~~~~~~~~~~~
Matching a let-expression.  Consider
        RULE forall x.  f (g x) = <rhs>
and target expression
        f (let { w=R } in g E))
Then we'd like the rule to match, to generate
        let { w=R } in (\x. <rhs>) E
In effect, we want to float the let-binding outward, to enable
the match to happen.  This is the WHOLE REASON for accumulating
bindings in the RuleSubst

We can only do this if the free variables of R are not bound by the
part of the target expression outside the let binding; e.g.
        f (\v. let w = v+1 in g E)
Here we obviously cannot float the let-binding for w.  Hence the
use of okToFloat.

There are a couple of tricky points.
  (a) What if floating the binding captures a variable?
        f (let v = x+1 in v) v
      --> NOT!
        let v = x+1 in f (x+1) v

  (b) What if two non-nested let bindings bind the same variable?
        f (let v = e1 in b1) (let v = e2 in b2)
      --> NOT!
        let v = e1 in let v = e2 in (f b2 b2)
      See testsuite test "RuleFloatLet".

Our cunning plan is this:
  * Along with the growing substitution for template variables
    we maintain a growing set of floated let-bindings (rs_binds)
    plus the set of variables thus bound.

  * The RnEnv2 in the MatchEnv binds only the local binders
    in the term (lambdas, case)

  * When we encounter a let in the term to be matched, we
    check that does not mention any locally bound (lambda, case)
    variables.  If so we fail

  * We use CoreSubst.substBind to freshen the binding, using an
    in-scope set that is the original in-scope variables plus the
    rs_bndrs (currently floated let-bindings).  So in (a) above
    we'll freshen the 'v' binding; in (b) above we'll freshen
    the *second* 'v' binding.

  * We apply that freshening substitution, in a lexically-scoped
    way to the term, although lazily; this is the rv_fltR field.


Note [Matching cases]
~~~~~~~~~~~~~~~~~~~~~
{- NOTE: This idea is currently disabled.  It really only works if
         the primops involved are OkForSpeculation, and, since
         they have side effects readIntOfAddr and touch are not.
         Maybe we'll get back to this later .  -}

Consider
   f (case readIntOffAddr# p# i# realWorld# of { (# s#, n# #) ->
      case touch# fp s# of { _ ->
      I# n# } } )
This happened in a tight loop generated by stream fusion that
Roman encountered.  We'd like to treat this just like the let
case, because the primops concerned are ok-for-speculation.
That is, we'd like to behave as if it had been
   case readIntOffAddr# p# i# realWorld# of { (# s#, n# #) ->
   case touch# fp s# of { _ ->
   f (I# n# } } )

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


************************************************************************
*                                                                      *
                   Rule-check the program
*                                                                      *
************************************************************************

   We want to know what sites have rules that could have fired but didn't.
   This pass runs over the tree (without changing it) and reports such.
-}

-- | Report partial matches for rules beginning with the specified
-- string for the purposes of error reporting
ruleCheckProgram :: CompilerPhase               -- ^ Rule activation test
                 -> String                      -- ^ Rule pattern
                 -> (Id -> [CoreRule])          -- ^ Rules for an Id
                 -> CoreProgram                 -- ^ Bindings to check in
                 -> SDoc                        -- ^ Resulting check message
ruleCheckProgram phase rule_pat rules binds
  | isEmptyBag results
  = text "Rule check results: no rule application sites"
  | otherwise
  = vcat [text "Rule check results:",
          line,
          vcat [ p $$ line | p <- bagToList results ]
         ]
  where
    env = RuleCheckEnv { rc_is_active = isActive phase
                       , rc_id_unf    = idUnfolding     -- Not quite right
                                                        -- Should use activeUnfolding
                       , rc_pattern   = rule_pat
                       , rc_rules = rules }
    results = unionManyBags (map (ruleCheckBind env) binds)
    line = text (replicate 20 '-')

data RuleCheckEnv = RuleCheckEnv {
    rc_is_active :: Activation -> Bool,
    rc_id_unf  :: IdUnfoldingFun,
    rc_pattern :: String,
    rc_rules :: Id -> [CoreRule]
}

ruleCheckBind :: RuleCheckEnv -> CoreBind -> Bag SDoc
   -- The Bag returned has one SDoc for each call site found
ruleCheckBind env (NonRec _ r) = ruleCheck env r
ruleCheckBind env (Rec prs)    = unionManyBags [ruleCheck env r | (_,r) <- prs]

ruleCheck :: RuleCheckEnv -> CoreExpr -> Bag SDoc
ruleCheck _   (Var _)       = emptyBag
ruleCheck _   (Lit _)       = emptyBag
ruleCheck _   (Type _)      = emptyBag
ruleCheck _   (Coercion _)  = emptyBag
ruleCheck env (App f a)     = ruleCheckApp env (App f a) []
ruleCheck env (Tick _ e)  = ruleCheck env e
ruleCheck env (Cast e _)    = ruleCheck env e
ruleCheck env (Let bd e)    = ruleCheckBind env bd `unionBags` ruleCheck env e
ruleCheck env (Lam _ e)     = ruleCheck env e
ruleCheck env (Case e _ _ as) = ruleCheck env e `unionBags`
                                unionManyBags [ruleCheck env r | (_,_,r) <- as]

ruleCheckApp :: RuleCheckEnv -> Expr CoreBndr -> [Arg CoreBndr] -> Bag SDoc
ruleCheckApp env (App f a) as = ruleCheck env a `unionBags` ruleCheckApp env f (a:as)
ruleCheckApp env (Var f) as   = ruleCheckFun env f as
ruleCheckApp env other _      = ruleCheck env other

ruleCheckFun :: RuleCheckEnv -> Id -> [CoreExpr] -> Bag SDoc
-- Produce a report for all rules matching the predicate
-- saying why it doesn't match the specified application

ruleCheckFun env fn args
  | null name_match_rules = emptyBag
  | otherwise             = unitBag (ruleAppCheck_help env fn args name_match_rules)
  where
    name_match_rules = filter match (rc_rules env fn)
    match rule = (rc_pattern env) `isPrefixOf` unpackFS (ruleName rule)

ruleAppCheck_help :: RuleCheckEnv -> Id -> [CoreExpr] -> [CoreRule] -> SDoc
ruleAppCheck_help env fn args rules
  =     -- The rules match the pattern, so we want to print something
    vcat [text "Expression:" <+> ppr (mkApps (Var fn) args),
          vcat (map check_rule rules)]
  where
    n_args = length args
    i_args = args `zip` [1::Int ..]
    rough_args = map roughTopName args

    check_rule rule = sdocWithDynFlags $ \dflags ->
                      rule_herald rule <> colon <+> rule_info dflags rule

    rule_herald (BuiltinRule { ru_name = name })
        = text "Builtin rule" <+> doubleQuotes (ftext name)
    rule_herald (Rule { ru_name = name })
        = text "Rule" <+> doubleQuotes (ftext name)

    rule_info dflags rule
        | Just _ <- matchRule dflags (emptyInScopeSet, rc_id_unf env)
                              noBlackList fn args rough_args rule
        = text "matches (which is very peculiar!)"

    rule_info _ (BuiltinRule {}) = text "does not match"

    rule_info _ (Rule { ru_act = act,
                        ru_bndrs = rule_bndrs, ru_args = rule_args})
        | not (rc_is_active env act)  = text "active only in later phase"
        | n_args < n_rule_args        = text "too few arguments"
        | n_mismatches == n_rule_args = text "no arguments match"
        | n_mismatches == 0           = text "all arguments match (considered individually), but rule as a whole does not"
        | otherwise                   = text "arguments" <+> ppr mismatches <+> text "do not match (1-indexing)"
        where
          n_rule_args  = length rule_args
          n_mismatches = length mismatches
          mismatches   = [i | (rule_arg, (arg,i)) <- rule_args `zip` i_args,
                              not (isJust (match_fn rule_arg arg))]

          lhs_fvs = exprsFreeVars rule_args     -- Includes template tyvars
          match_fn rule_arg arg = match renv emptyRuleSubst rule_arg arg
                where
                  in_scope = mkInScopeSet (lhs_fvs `unionVarSet` exprFreeVars arg)
                  renv = RV { rv_lcl   = mkRnEnv2 in_scope
                            , rv_tmpls = mkVarSet rule_bndrs
                            , rv_fltR  = mkEmptySubst in_scope
                            , rv_unf   = rc_id_unf env }

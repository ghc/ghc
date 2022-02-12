{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[CoreRules]{Rewrite rules}
-}


-- | Functions for collecting together and applying rewrite rules to a module.
-- The 'CoreRule' datatype itself is declared elsewhere.
module GHC.Core.Rules (
        -- ** Constructing
        emptyRuleBase, mkRuleBase, extendRuleBaseList,
        pprRuleBase, extendRuleEnv,

        -- ** Checking rule applications
        ruleCheckProgram,

        -- ** Manipulating 'RuleInfo' rules
        extendRuleInfo, addRuleInfo,
        addIdSpecialisations,

        -- * Misc. CoreRule helpers
        rulesOfBinds, getRules, pprRulesForUser,

        lookupRule, mkRule, roughTopNames, initRuleOpts
    ) where

import GHC.Prelude

import GHC.Driver.Session      ( DynFlags, gopt, targetPlatform, homeUnitId_ )
import GHC.Driver.Flags

import GHC.Unit.Types    ( primUnitId, bignumUnitId )
import GHC.Unit.Module   ( Module )
import GHC.Unit.Module.Env

import GHC.Core         -- All of it
import GHC.Core.Subst
import GHC.Core.SimpleOpt ( exprIsLambda_maybe )
import GHC.Core.FVs       ( exprFreeVars, exprsFreeVars, bindFreeVars
                          , rulesFreeVarsDSet, exprsOrphNames, exprFreeVarsList )
import GHC.Core.Utils     ( exprType, mkTick, mkTicks
                          , stripTicksTopT, stripTicksTopE
                          , isJoinBind, mkCastMCo )
import GHC.Core.Ppr       ( pprRules )
import GHC.Core.Unify as Unify ( ruleMatchTyKiX )
import GHC.Core.Type as Type
   ( Type, TCvSubst, extendTvSubst, extendCvSubst
   , mkEmptyTCvSubst, substTy, getTyVar_maybe )
import GHC.Core.Coercion as Coercion
import GHC.Core.Tidy     ( tidyRules )
import GHC.Core.Map.Expr ( eqCoreExpr )

import GHC.Tc.Utils.TcType  ( tcSplitTyConApp_maybe )
import GHC.Builtin.Types    ( anyTypeOfKind )

import GHC.Types.Id
import GHC.Types.Id.Info ( RuleInfo( RuleInfo ) )
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name    ( Name, NamedThing(..), nameIsLocalOrFrom )
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Unique.FM
import GHC.Types.Tickish
import GHC.Types.Basic

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.Bag

import GHC.Utils.Misc as Utils
import GHC.Utils.Trace
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

import Data.List (sortBy, mapAccumL, isPrefixOf)
import Data.Function    ( on )
import Control.Monad    ( guard )

{-
Note [Overall plumbing for rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* After the desugarer:
   - The ModGuts initially contains mg_rules :: [CoreRule] of
     locally-declared rules for imported Ids.
   - Locally-declared rules for locally-declared Ids are attached to
     the IdInfo for that Id.  See Note [Attach rules to local ids] in
     GHC.HsToCore.Binds

* GHC.Iface.Tidy strips off all the rules from local Ids and adds them to
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

* At the moment (c) is carried in a reader-monad way by the GHC.Core.Opt.Monad.
  The HomePackageTable doesn't have a single RuleBase because technically
  we should only be able to "see" rules "below" this module; so we
  generate a RuleBase for (c) by combing rules from all the modules
  "below" us.  That's why we can't just select the home-package RuleBase
  from HscEnv.

  [NB: we are inconsistent here.  We should do the same for external
  packages, but we don't.  Same for type-class instances.]

* So in the outer simplifier loop (simplifyPgmIO), we combine (b & c) into a single
  RuleBase, reading
     (b) from the ModGuts,
     (c) from the GHC.Core.Opt.Monad, and
  just before doing rule matching we read
     (d) from its mutable variable
  and combine it with the results from (b & c).

  In a single simplifier run new rules can be added into the EPS so it matters
  to keep an up-to-date view of which rules have been loaded. For examples of
  where this went wrong and caused cryptic performance regressions seee
  see T19790 and !6735.


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
-- compiled. See also 'GHC.Core.CoreRule'
mkRule this_mod is_auto is_local name act fn bndrs args rhs
  = Rule { ru_name = name, ru_fn = fn, ru_act = act,
           ru_bndrs = bndrs, ru_args = args,
           ru_rhs = rhs,
           ru_rough = roughTopNames args,
           ru_origin = this_mod,
           ru_orphan = orph,
           ru_auto = is_auto, ru_local = is_local }
  where
        -- Compute orphanhood.  See Note [Orphans] in GHC.Core.InstEnv
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
--    (if that name is a GlobalId: see "GHC.Types.Var#globalvslocal"), or
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

pprRulesForUser :: [CoreRule] -> SDoc
-- (a) tidy the rules
-- (b) sort them into order based on the rule name
-- (c) suppress uniques (unless -dppr-debug is on)
-- This combination makes the output stable so we can use in testing
-- It's here rather than in GHC.Core.Ppr because it calls tidyRules
pprRulesForUser rules
  = withPprStyle defaultUserStyle $
    pprRules $
    sortBy (lexicalCompareFS `on` ruleName) $
    tidyRules emptyTidyEnv rules

{-
************************************************************************
*                                                                      *
                RuleInfo: the rules in an IdInfo
*                                                                      *
************************************************************************
-}

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
  = idCoreRules fn ++ concatMap imp_rules rule_base
  where
    imp_rules rb = filter (ruleIsVisible orphs) (lookupNameEnv rb (idName fn) `orElse` [])

ruleIsVisible :: ModuleSet -> CoreRule -> Bool
ruleIsVisible _ BuiltinRule{} = True
ruleIsVisible vis_orphs Rule { ru_orphan = orph, ru_origin = origin }
    = notOrphan orph || origin `elemModuleSet` vis_orphs

{- Note [Where rules are found]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rules for an Id come from two places:
  (a) the ones it is born with, stored inside the Id itself (idCoreRules fn),
  (b) rules added in other modules, stored in the global RuleBase (imp_rules)

It's tempting to think that
     - LocalIds have only (a)
     - non-LocalIds have only (b)

but that isn't quite right:

     - PrimOps and ClassOps are born with a bunch of rules inside the Id,
       even when they are imported

     - The rules in GHC.Core.Opt.ConstantFold.builtinRules should be active even
       in the module defining the Id (when it's a LocalId), but
       the rules are kept in the global RuleBase


************************************************************************
*                                                                      *
                RuleBase
*                                                                      *
************************************************************************
-}

-- RuleBase itself is defined in GHC.Core, along with CoreRule

emptyRuleBase :: RuleBase
emptyRuleBase = emptyNameEnv

mkRuleBase :: [CoreRule] -> RuleBase
mkRuleBase rules = extendRuleBaseList emptyRuleBase rules

extendRuleBaseList :: RuleBase -> [CoreRule] -> RuleBase
extendRuleBaseList rule_base new_guys
  = foldl' extendRuleBase rule_base new_guys

extendRuleBase :: RuleBase -> CoreRule -> RuleBase
extendRuleBase rule_base rule
  = extendNameEnv_Acc (:) Utils.singleton rule_base (ruleIdName rule) rule

extendRuleEnv :: RuleEnv -> RuleBase -> RuleEnv
extendRuleEnv (RuleEnv rules orphs) rb = (RuleEnv (rb:rules) orphs)

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
lookupRule :: RuleOpts -> InScopeEnv
           -> (Activation -> Bool)      -- When rule is active
           -> Id -- Function head
           -> [CoreExpr] -- Args
           -> [CoreRule] -- Rules
           -> Maybe (CoreRule, CoreExpr)

-- See Note [Extra args in the target]
-- See comments on matchRule
lookupRule opts rule_env@(in_scope,_) is_active fn args rules
  = -- pprTrace "matchRules" (ppr fn <+> ppr args $$ ppr rules ) $
    case go [] rules of
        []     -> Nothing
        (m:ms) -> Just (findBest in_scope (fn,args') m ms)
  where
    rough_args = map roughTopName args

    -- Strip ticks from arguments, see Note [Tick annotations in RULE
    -- matching]. We only collect ticks if a rule actually matches -
    -- this matters for performance tests.
    args' = map (stripTicksTopE tickishFloatable) args
    ticks = concatMap (stripTicksTopT tickishFloatable) args

    go :: [(CoreRule,CoreExpr)] -> [CoreRule] -> [(CoreRule,CoreExpr)]
    go ms [] = ms
    go ms (r:rs)
      | Just e <- matchRule opts rule_env is_active fn args' rough_args r
      = go ((r,mkTicks ticks e):ms) rs
      | otherwise
      = -- pprTrace "match failed" (ppr r $$ ppr args $$
        --   ppr [ (arg_id, unfoldingTemplate unf)
        --       | Var arg_id <- args
        --       , let unf = idUnfolding arg_id
        --       , isCheapUnfolding unf] )
        go ms rs

findBest :: InScopeSet -> (Id, [CoreExpr])
         -> (CoreRule,CoreExpr) -> [(CoreRule,CoreExpr)] -> (CoreRule,CoreExpr)
-- All these pairs matched the expression
-- Return the pair the most specific rule
-- The (fn,args) is just for overlap reporting

findBest _        _      (rule,ans)   [] = (rule,ans)
findBest in_scope target (rule1,ans1) ((rule2,ans2):prs)
  | isMoreSpecific in_scope rule1 rule2 = findBest in_scope target (rule1,ans1) prs
  | isMoreSpecific in_scope rule2 rule1 = findBest in_scope target (rule2,ans2) prs
  | debugIsOn = let pp_rule rule
                      = ifPprDebug (ppr rule)
                                   (doubleQuotes (ftext (ruleName rule)))
                in pprTrace "Rules.findBest: rule overlap (Rule 1 wins)"
                         (vcat [ whenPprDebug $
                                 text "Expression to match:" <+> ppr fn
                                 <+> sep (map ppr args)
                               , text "Rule 1:" <+> pp_rule rule1
                               , text "Rule 2:" <+> pp_rule rule2]) $
                findBest in_scope target (rule1,ans1) prs
  | otherwise = findBest in_scope target (rule1,ans1) prs
  where
    (fn,args) = target

isMoreSpecific :: InScopeSet -> CoreRule -> CoreRule -> Bool
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
isMoreSpecific _        (BuiltinRule {}) _                = False
isMoreSpecific _        (Rule {})        (BuiltinRule {}) = True
isMoreSpecific in_scope (Rule { ru_bndrs = bndrs1, ru_args = args1 })
                        (Rule { ru_bndrs = bndrs2, ru_args = args2
                              , ru_name = rule_name2, ru_rhs = rhs2 })
  = isJust (matchN (full_in_scope, id_unfolding_fun)
                   rule_name2 bndrs2 args2 args1 rhs2)
  where
   id_unfolding_fun _ = NoUnfolding     -- Don't expand in templates
   full_in_scope = in_scope `extendInScopeSetList` bndrs1

noBlackList :: Activation -> Bool
noBlackList _ = False           -- Nothing is black listed

{- Note [Extra args in the target]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we find a matching rule, we return (Just (rule, rhs)),
/but/ the rule firing has only consumed as many of the input args
as the ruleArity says.  The unused arguments are handled by the code in
GHC.Core.Opt.Simplify.tryRules, using the arity of the returned rule.

E.g. Rule "foo":  forall a b.  f p1 p2 = rhs
     Target:      f e1 e2 e3

Then lookupRule returns Just (Rule "foo", rhs), where Rule "foo"
has ruleArity 2.  The real rewrite is
        f e1 e2 e3 ==> rhs e3

You might think it'd be cleaner for lookupRule to deal with the
leftover arguments, by applying 'rhs' to them, but the main call
in the Simplifier works better as it is.  Reason: the 'args' passed
to lookupRule are the result of a lazy substitution

Historical note:

At one stage I tried to match even if there are more args in the
/template/ than the target.  I now think this is probably a bad idea.
Should the template (map f xs) match (map g)?  I think not.  For a
start, in general eta expansion wastes work.  SLPJ July 99
-}

------------------------------------
matchRule :: RuleOpts -> InScopeEnv -> (Activation -> Bool)
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

matchRule opts rule_env _is_active fn args _rough_args
          (BuiltinRule { ru_try = match_fn })
-- Built-in rules can't be switched off, it seems
  = case match_fn opts rule_env fn args of
        Nothing   -> Nothing
        Just expr -> Just expr

matchRule _ rule_env is_active _ args rough_args
          (Rule { ru_name = rule_name, ru_act = act, ru_rough = tpl_tops
                , ru_bndrs = tpl_vars, ru_args = tpl_args, ru_rhs = rhs })
  | not (is_active act)               = Nothing
  | ruleCantMatch tpl_tops rough_args = Nothing
  | otherwise = matchN rule_env rule_name tpl_vars tpl_args args rhs


-- | Initialize RuleOpts from DynFlags
initRuleOpts :: DynFlags -> RuleOpts
initRuleOpts dflags = RuleOpts
  { roPlatform                = targetPlatform dflags
  , roNumConstantFolding      = gopt Opt_NumConstantFolding dflags
  , roExcessRationalPrecision = gopt Opt_ExcessPrecision dflags
    -- disable bignum rules in ghc-prim and ghc-bignum itself
  , roBignumRules             = homeUnitId_ dflags /= primUnitId
                                && homeUnitId_ dflags /= bignumUnitId
  }


---------------------------------------
matchN  :: InScopeEnv
        -> RuleName -> [Var] -> [CoreExpr]
        -> [CoreExpr] -> CoreExpr           -- ^ Target; can have more elements than the template
        -> Maybe CoreExpr
-- For a given match template and context, find bindings to wrap around
-- the entire result and what should be substituted for each template variable.
--
-- Fail if there are too few actual arguments from the target to match the template
--
-- See Note [Extra args in the target]
-- If there are too /many/ actual arguments, we simply ignore the
-- trailing ones, returning the result of applying the rule to a prefix
-- of the actual arguments.

matchN (in_scope, id_unf) rule_name tmpl_vars tmpl_es target_es rhs
  = do  { rule_subst <- match_exprs init_menv emptyRuleSubst tmpl_es target_es
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

data RuleMatchEnv
  = RV { rv_lcl   :: RnEnv2          -- Renamings for *local bindings*
                                     --   (lambda/case)
       , rv_tmpls :: VarSet          -- Template variables
                                     --   (after applying envL of rv_lcl)
       , rv_fltR  :: Subst           -- Renamings for floated let-bindings
                                     --   (domain disjoint from envR of rv_lcl)
                                     -- See Note [Matching lets]
                                     -- N.B. The InScopeSet of rv_fltR is always ignored;
                                     -- see (4) in Note [Matching lets].
       , rv_unf :: IdUnfoldingFun
       }

{- Note [rv_lcl in RuleMatchEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider matching
  Template: \x->f
  Target:   \f->f

where 'f' is free in the template. When we meet the lambdas we must
remember to rename f :-> f' in the target, as well as x :-> f
in the template.  The rv_lcl::RnEnv2 does that.

Similarly, consider matching
     Template: {a}  \b->b
     Target:        \a->3
We must rename the \a.  Otherwise when we meet the lambdas we might
substitute [b :-> a] in the template, and then erroneously succeed in
matching what looks like the template variable 'a' against 3.

So we must add the template vars to the in-scope set before starting;
see `init_menv` in `matchN`.
-}

rvInScopeEnv :: RuleMatchEnv -> InScopeEnv
rvInScopeEnv renv = (rnInScopeSet (rv_lcl renv), rv_unf renv)

-- * The domain of the TvSubstEnv and IdSubstEnv are the template
--   variables passed into the match.
--
-- * The BindWrapper in a RuleSubst are the bindings floated out
--   from nested matches; see the Let case of match, below
--
data RuleSubst = RS { rs_tv_subst :: TvSubstEnv   -- Range is the
                    , rs_id_subst :: IdSubstEnv   --   template variables
                    , rs_binds    :: BindWrapper  -- Floated bindings
                    , rs_bndrs    :: [Var]        -- Variables bound by floated lets
                    }

type BindWrapper = CoreExpr -> CoreExpr
  -- See Notes [Matching lets] and [Matching cases]
  -- we represent the floated bindings as a core-to-core function

emptyRuleSubst :: RuleSubst
emptyRuleSubst = RS { rs_tv_subst = emptyVarEnv, rs_id_subst = emptyVarEnv
                    , rs_binds = \e -> e, rs_bndrs = [] }

----------------------
match_exprs :: RuleMatchEnv -> RuleSubst
            -> [CoreExpr]       -- Templates
            -> [CoreExpr]       -- Targets
            -> Maybe RuleSubst
-- If the targets are longer than templates, succeed, simply ignoring
-- the leftover targets. This matters in the call in matchN.
--
-- Precondition: corresponding elements of es1 and es2 have the same
--               type, assumuing earlier elements match
-- Example:  f :: forall v. v -> blah
--   match_exprs [Type a, y::a] [Type Int, 3]
-- Then, after matching Type a against Type Int,
-- the type of (y::a) matches that of (3::Int)
match_exprs _ subst [] _
  = Just subst
match_exprs renv subst (e1:es1) (e2:es2)
  = do { subst' <- match renv subst e1 e2 MRefl
       ; match_exprs renv subst' es1 es2 }
match_exprs _ _ _ _ = Nothing


{- Note [Casts in the target]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As far as possible we don't want casts in the target to get in the way of
matching.  E.g.
* (let bind in e)  |> co
* (case e of alts) |> co
* (\ a b. f a b)   |> co

In the first two cases we want to float the cast inwards so we can match on
the let/case.  This is not important in practice because the Simplifier does
this anyway.

But the third case /is/ important: we don't want the cast to get in the way
of eta-reduction.  See Note [Cancel reflexive casts] for a real life example.

The most convenient thing is to make 'match' take an MCoercion argument, thus:

* The main matching function
      match env subst template target mco
  matches   template ~ (target |> mco)

* Invariant: typeof( subst(template) ) = typeof( target |> mco )

Note that for applications
     (e1 e2) ~ (d1 d2) |> co
where 'co' is non-reflexive, we simply fail.  You might wonder about
     (e1 e2) ~ ((d1 |> co1) d2) |> co2
but the Simplifer pushes the casts in an application to to the
right, if it can, so this doesn't really arise.

Note [Coercion arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~
What if we have (f co) in the template, where the 'co' is a coercion
argument to f?  Right now we have nothing in place to ensure that a
coercion /argument/ in the template is a variable.  We really should,
perhaps by abstracting over that variable.

C.f. the treatment of dictionaries in GHC.HsToCore.Binds.decompseRuleLhs.

For now, though, we simply behave badly, by failing in match_co.
We really should never rely on matching the structure of a coercion
(which is just a proof).

Note [Casts in the template]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the definition
  f x = e,
and SpecConstr on call pattern
  f ((e1,e2) |> co)

We'll make a RULE
   RULE forall a,b,g.  f ((a,b)|> g) = $sf a b g
   $sf a b g = e[ ((a,b)|> g) / x ]

So here is the invariant:

  In the template, in a cast (e |> co),
  the cast `co` is always a /variable/.

Matching should bind that variable to an actual coercion, so that we
can use it in $sf.  So a Cast on the LHS (the template) calls
match_co, which succeeds when the template cast is a variable -- which
it always is.  That is why match_co has so few cases.

See also
* Note [Coercion arguments]
* Note [Matching coercion variables] in GHC.Core.Unify.
* Note [Cast swizzling on rule LHSs] in GHC.Core.Opt.Simplify.Utils:
  sm_cast_swizzle is switched off in the template of a RULE
-}

----------------------
match :: RuleMatchEnv
      -> RuleSubst              -- Substitution applies to template only
      -> CoreExpr               -- Template
      -> CoreExpr               -- Target
      -> MCoercion
      -> Maybe RuleSubst

-- Postcondition (TypeInv): if matching succeeds, then
--                          typeof( subst(template) ) = typeof( target |> mco )
--     But this is /not/ a pre-condition! The types of template and target
--     may differ, see the (App e1 e2) case
--
-- Invariant (CoInv):   if mco :: ty ~ ty, then it is MRefl, not MCo co
--                      See Note [Cancel reflexive casts]
--
-- See the notes with Unify.match, which matches types
-- Everything is very similar for terms


------------------------ Ticks ---------------------
-- We look through certain ticks. See Note [Tick annotations in RULE matching]
match renv subst e1 (Tick t e2) mco
  | tickishFloatable t
  = match renv subst' e1 e2 mco
  | otherwise
  = Nothing
  where
    subst' = subst { rs_binds = rs_binds subst . mkTick t }

match renv subst e@(Tick t e1) e2 mco
  | tickishFloatable t  -- Ignore floatable ticks in rule template.
  =  match renv subst e1 e2 mco
  | otherwise
  = pprPanic "Tick in rule" (ppr e)

------------------------ Types ---------------------
match renv subst (Type ty1) (Type ty2) _mco
  = match_ty renv subst ty1 ty2

------------------------ Coercions ---------------------
-- See Note [Coercion arguments] for why this isn't really right
match renv subst (Coercion co1) (Coercion co2) MRefl
  = match_co renv subst co1 co2
  -- The MCo case corresponds to matching  co ~ (co2 |> co3)
  -- and I have no idea what to do there -- or even if it can occur
  -- Failing seems the simplest thing to do; it's certainly safe.

------------------------ Casts ---------------------
-- See Note [Casts in the template]
--     Note [Casts in the target]
--     Note [Cancel reflexive casts]

match renv subst e1 (Cast e2 co2) mco
  = match renv subst e1 e2 (checkReflexiveMCo (mkTransMCoR co2 mco))
    -- checkReflexiveMCo: cancel casts if possible
    -- This is important: see Note [Cancel reflexive casts]

match renv subst (Cast e1 co1) e2 mco
  = -- See Note [Casts in the template]
    do { let co2 = case mco of
                     MRefl   -> mkRepReflCo (exprType e2)
                     MCo co2 -> co2
       ; subst1 <- match_co renv subst co1 co2
         -- If match_co succeeds, then (exprType e1) = (exprType e2)
         -- Hence the MRefl in the next line
       ; match renv subst1 e1 e2 MRefl }

------------------------ Literals ---------------------
match _ subst (Lit lit1) (Lit lit2) mco
  | lit1 == lit2
  = assertPpr (isReflMCo mco) (ppr mco) $
    Just subst

------------------------ Variables ---------------------
-- The Var case follows closely what happens in GHC.Core.Unify.match
match renv subst (Var v1) e2 mco
  = match_var renv subst v1 (mkCastMCo e2 mco)

match renv subst e1 (Var v2) mco  -- Note [Expanding variables]
  | not (inRnEnvR rn_env v2)      -- Note [Do not expand locally-bound variables]
  , Just e2' <- expandUnfolding_maybe (rv_unf renv v2')
  = match (renv { rv_lcl = nukeRnEnvR rn_env }) subst e1 e2' mco
  where
    v2'    = lookupRnInScope rn_env v2
    rn_env = rv_lcl renv
        -- Notice that we look up v2 in the in-scope set
        -- See Note [Lookup in-scope]
        -- No need to apply any renaming first (hence no rnOccR)
        -- because of the not-inRnEnvR

------------------------ Applications ---------------------
-- Note the match on MRefl!  We fail if there is a cast in the target
--     (e1 e2) ~ (d1 d2) |> co
-- See Note [Cancel reflexive casts]: in the Cast equations for 'match'
-- we agressively ensure that if MCo is reflective, it really is MRefl.
match renv subst (App f1 a1) (App f2 a2) MRefl
  = do  { subst' <- match renv subst f1 f2 MRefl
        ; match renv subst' a1 a2 MRefl }

------------------------ Float lets ---------------------
match renv subst e1 (Let bind e2) mco
  | -- pprTrace "match:Let" (vcat [ppr bind, ppr $ okToFloat (rv_lcl renv) (bindFreeVars bind)]) $
    not (isJoinBind bind) -- can't float join point out of argument position
  , okToFloat (rv_lcl renv) (bindFreeVars bind) -- See Note [Matching lets]
  = match (renv { rv_fltR = flt_subst'
                , rv_lcl  = rv_lcl renv `extendRnInScopeSetList` new_bndrs })
                -- We are floating the let-binding out, as if it had enclosed
                -- the entire target from Day 1.  So we must add its binders to
                -- the in-scope set (#20200)
          (subst { rs_binds = rs_binds subst . Let bind'
                 , rs_bndrs = new_bndrs ++ rs_bndrs subst })
          e1 e2 mco
  | otherwise
  = Nothing
  where
    in_scope  = rnInScopeSet (rv_lcl renv) `extendInScopeSetList` rs_bndrs subst
                -- in_scope: see (4) in Note [Matching lets]
    flt_subst = rv_fltR renv `setInScope` in_scope
    (flt_subst', bind') = substBind flt_subst bind
    new_bndrs           = bindersOf bind'

------------------------  Lambdas ---------------------
match renv subst (Lam x1 e1) e2 mco
  | Just (x2, e2', ts) <- exprIsLambda_maybe (rvInScopeEnv renv) (mkCastMCo e2 mco)
    -- See Note [Lambdas in the template]
  = let renv'  = rnMatchBndr2 renv x1 x2
        subst' = subst { rs_binds = rs_binds subst . flip (foldr mkTick) ts }
    in  match renv' subst' e1 e2' MRefl

match renv subst e1 e2@(Lam {}) mco
  | Just (renv', e2') <- eta_reduce renv e2  -- See Note [Eta reduction in the target]
  = match renv' subst e1 e2' mco

{- Note [Lambdas in the template]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we match
   Template:   (\x. blah_template)
   Target:     (\y. blah_target)
then we want to match inside the lambdas, using rv_lcl to match up
x and y.

But what about this?
   Template   (\x. (blah1 |> cv))
   Target     (\y. blah2) |> co

This happens quite readily, because the Simplifier generally moves
casts outside lambdas: see Note [Casts and lambdas] in
GHC.Core.Opt.Simplify.Utils. So, tiresomely, we want to push `co`
back inside, which is what `exprIsLambda_maybe` does.  But we've
stripped off that cast, so now we need to put it back, hence mkCastMCo.

Unlike the target, where we attempt eta-reduction, we do not attempt
to eta-reduce the template, and may therefore fail on
  Template:   \x. f True x
  Target      f True

It's not especially easy to deal with eta reducing the template,
and never happens, because no one write eta-expanded left-hand-sides.
-}

------------------------ Case expression ---------------------
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

match renv subst (Case e1 x1 ty1 alts1) (Case e2 x2 ty2 alts2) mco
  = do  { subst1 <- match_ty renv subst ty1 ty2
        ; subst2 <- match renv subst1 e1 e2 MRefl
        ; let renv' = rnMatchBndr2 renv x1 x2
        ; match_alts renv' subst2 alts1 alts2 mco   -- Alts are both sorted
        }

-- Everything else fails
match _ _ _e1 _e2 _mco = -- pprTrace "Failing at" ((text "e1:" <+> ppr _e1) $$ (text "e2:" <+> ppr _e2)) $
                         Nothing

-------------
eta_reduce :: RuleMatchEnv -> CoreExpr -> Maybe (RuleMatchEnv, CoreExpr)
-- See Note [Eta reduction in the target]
eta_reduce renv e@(Lam {})
  = go renv id [] e
  where
    go :: RuleMatchEnv -> BindWrapper -> [Var] -> CoreExpr
       -> Maybe (RuleMatchEnv, CoreExpr)
    go renv bw vs (Let b e) = go renv (bw . Let b) vs e

    go renv bw vs (Lam v e) = go renv' bw (v':vs) e
      where
         (rn_env', v') = rnBndrR (rv_lcl renv) v
         renv' = renv { rv_lcl = rn_env' }

    go renv bw (v:vs) (App f arg)
      | Var a <- arg, v == rnOccR (rv_lcl renv) a
      = go renv bw vs f

      | Type ty <- arg, Just tv <- getTyVar_maybe ty
      , v == rnOccR (rv_lcl renv) tv
      = go renv bw vs f

    go renv bw []    e = Just (renv, bw e)
    go _    _  (_:_) _ = Nothing

eta_reduce _ _ = Nothing

{- Note [Eta reduction in the target]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are faced with this (#19790)
   Template {x}  f x
   Target        (\a b c. let blah in f x a b c)

You might wonder why we have an eta-expanded target (see first subtle
point below), but regardless of how it came about, we'd like
eta-expansion not to impede matching.

So eta_reduce does on-the-fly eta-reduction of the target expression.
Given (\a b c. let blah in e a b c), it returns (let blah in e).

Subtle points:
* Consider a target:  \x. f <expensive> x
  In the main eta-reducer we do not eta-reduce this, because doing so
  might reduce the arity of the expression (from 1 to zero, because of
  <expensive>).  But for rule-matching we /do/ want to match template
  (f a) against target (\x. f <expensive> x), with a := <expensive>

  This is a compelling reason for not relying on the Simplifier's
  eta-reducer.

* The Lam case of eta_reduce renames as it goes. Consider
  (\x. \x. f x x).  We should not eta-reduce this.  As we go we rename
  the first x to x1, and the second to x2; then both argument x's are x2.

* eta_reduce does /not/ need to check that the bindings 'blah'
  and expression 'e' don't mention a b c; but it /does/ extend the
  rv_lcl RnEnv2 (see rn_bndr in eta_reduce).
  * If 'blah' mentions the binders, the let-float rule won't
    fire; and
  * if 'e' mentions the binders we we'll also fail to match
    e.g. because of the exprFreeVars test in match_tmpl_var.

  Example: Template: {x}  f a         -- Some top-level 'a'
           Target:   (\a b. f a a b)  -- The \a shadows top level 'a'
  Then eta_reduce will /succeed/, with
      (rnEnvR = [a :-> a'], f a)
  The returned RnEnv will map [a :-> a'], where a' is fresh. (There is
  no need to rename 'b' because (in this example) it is not in scope.
  So it's as if we'd returned (f a') from eta_reduce; the renaming applied
  to the target is simply deferred.

Note [Cancel reflexive casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is an example (from #19790) which we want to catch
   (f x) ~ (\a b. (f x |> co) a b) |> sym co
where
   f :: Int -> Stream
   co :: Stream ~ T1 -> T2 -> T3

when we eta-reduce (\a b. blah a b) to 'blah', we'll get
  (f x) ~ (f x) |> co |> sym co

and we really want to spot that the co/sym-co cancels out.
Hence
  * We keep an invariant that the MCoercion is always MRefl
    if the MCoercion is reflextve
  * We maintain this invariant via the call to checkReflexiveMCo
    in the Cast case of 'match'.
-}

-------------
match_co :: RuleMatchEnv
         -> RuleSubst
         -> Coercion
         -> Coercion
         -> Maybe RuleSubst
-- We only match if the template is a coercion variable or Refl:
--   see Note [Casts in the template]
-- Like 'match' it is /not/ guaranteed that
--     coercionKind template  =  coercionKind target
-- But if match_co succeeds, it /is/ guaranteed that
--     coercionKind (subst template) = coercionKind target

match_co renv subst co1 co2
  | Just cv <- getCoVar_maybe co1
  = match_var renv subst cv (Coercion co2)

  | Just (ty1, r1) <- isReflCo_maybe co1
  = do { (ty2, r2) <- isReflCo_maybe co2
       ; guard (r1 == r2)
       ; match_ty renv subst ty1 ty2 }

  | debugIsOn
  = pprTrace "match_co: needs more cases" (ppr co1 $$ ppr co2) Nothing
    -- Currently just deals with CoVarCo and Refl

  | otherwise
  = Nothing

-------------
rnMatchBndr2 :: RuleMatchEnv -> Var -> Var -> RuleMatchEnv
rnMatchBndr2 renv x1 x2
  = renv { rv_lcl  = rnBndr2 (rv_lcl renv) x1 x2
         , rv_fltR = delBndr (rv_fltR renv) x2 }


------------------------------------------
match_alts :: RuleMatchEnv
           -> RuleSubst
           -> [CoreAlt]                 -- Template
           -> [CoreAlt] -> MCoercion    -- Target
           -> Maybe RuleSubst
match_alts _ subst [] [] _
  = return subst
match_alts renv subst (Alt c1 vs1 r1:alts1) (Alt c2 vs2 r2:alts2) mco
  | c1 == c2
  = do  { subst1 <- match renv' subst r1 r2 mco
        ; match_alts renv subst1 alts1 alts2 mco }
  where
    renv' = foldl' mb renv (vs1 `zip` vs2)
    mb renv (v1,v2) = rnMatchBndr2 renv v1 v2

match_alts _ _ _ _ _
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
          -> Var        -- Template
          -> CoreExpr   -- Target
          -> Maybe RuleSubst
match_var renv@(RV { rv_tmpls = tmpls, rv_lcl = rn_env, rv_fltR = flt_env })
          subst v1 e2
  | v1' `elemVarSet` tmpls
  = match_tmpl_var renv subst v1' e2

  | otherwise   -- v1' is not a template variable; check for an exact match with e2
  = case e2 of  -- Remember, envR of rn_env is disjoint from rv_fltR
       Var v2 | Just v2' <- rnOccR_maybe rn_env v2
              -> -- v2 was bound by a nested lambda or case
                 if v1' == v2' then Just subst
                               else Nothing

              -- v2 is not bound nestedly; it is free
              -- in the whole expression being matched
              -- So it will be in the InScopeSet for flt_env (#20200)
              | Var v2' <- lookupIdSubst flt_env v2
              , v1' == v2'
              -> Just subst
              | otherwise
              -> Nothing

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
               -> CoreExpr           -- Target
               -> Maybe RuleSubst

match_tmpl_var renv@(RV { rv_lcl = rn_env, rv_fltR = flt_env })
               subst@(RS { rs_id_subst = id_subst, rs_bndrs = let_bndrs })
               v1' e2
  | any (inRnEnvR rn_env) (exprFreeVarsList e2)
  = Nothing     -- Skolem-escape failure
                -- e.g. match forall a. (\x-> a x) against (\y. y y)

  | Just e1' <- lookupVarEnv id_subst v1'
  = if eqCoreExpr e1' e2'
    then Just subst
    else Nothing

  | otherwise   -- See Note [Matching variable types]
  = do { subst' <- match_ty renv subst (idType v1') (exprType e2)
       ; return (subst' { rs_id_subst = id_subst' }) }
  where
    -- e2' is the result of applying flt_env to e2
    e2' | null let_bndrs = e2
        | otherwise = substExpr flt_env e2

    id_subst' = extendVarEnv (rs_id_subst subst) v1' e2'
         -- No further renaming to do on e2',
         -- because no free var of e2' is in the rnEnvR of the envt

------------------------------------------
match_ty :: RuleMatchEnv
         -> RuleSubst
         -> Type                -- Template
         -> Type                -- Target
         -> Maybe RuleSubst
-- Matching Core types: use the matcher in GHC.Tc.Utils.TcType.
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

{- Note [Matching variable types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When matching x ~ e, where 'x' is a template variable, we must check that
x's type matches e's type, to establish (TypeInv).  For example
  forall (c::Char->Int) (x::Char).
     f (c x) = "RULE FIRED"
We must not match on, say (f (pred (3::Int))).

It's actually quite difficult to come up with an example that shows
you need type matching, esp since matching is left-to-right, so type
args get matched first.  But it's possible (e.g. simplrun008) and this
is the Right Thing to do.

An alternative would be to make (TypeInf) into a /pre-condition/.  It
is threatened only by the App rule.  So when matching an application
(e1 e2) ~ (d1 d2) would be to collect args of the application chain,
match the types of the head, then match arg-by-arg.

However that alternative seems a bit more complicated.  And by
matching types at variables we do one match_ty for each template
variable, rather than one for each application chain.  Usually there are
fewer template variables, although for simple rules it could be the other
way around.

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
We used to unconditionally look through ticks in both template and
expression being matched. This is actually illegal for counting or
cost-centre-scoped ticks, because we have no place to put them without
changing entry counts and/or costs. So now we just fail the match in
these cases.

On the other hand, where we are allowed to insert new cost into the
tick scope, we can float them upwards to the rule application site.

Moreover, we may encounter ticks in the template of a rule. There are a few
ways in which these may be introduced (e.g. #18162, #17619). Such ticks are
ignored by the matcher. See Note [Simplifying rules] in
GHC.Core.Opt.Simplify.Utils for details.

cf Note [Tick annotations in call patterns] in GHC.Core.Opt.SpecConstr


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

There are a couple of tricky points:
  (a) What if floating the binding captures a variable that is
      free in the entire expression?
        f (let v = x+1 in v) v
      --> NOT!
        let v = x+1 in f (x+1) v

  (b) What if the let shadows a local binding?
        f (\v -> (v, let v = x+1 in (v,v))
      --> NOT!
        let v = x+1 in f (\v -> (v, (v,v)))

  (c) What if two non-nested let bindings bind the same variable?
        f (let v = e1 in b1) (let v = e2 in b2)
      --> NOT!
        let v = e1 in let v = e2 in (f b2 b2)
      See testsuite test `T4814`.

Our cunning plan is this:
  (1) Along with the growing substitution for template variables
      we maintain a growing set of floated let-bindings (rs_binds)
      plus the set of variables thus bound (rs_bndrs).

  (2) The RnEnv2 in the MatchEnv binds only the local binders
      in the term (lambdas, case), not the floated let-bndrs.

  (3) When we encounter a `let` in the term to be matched, in the Let
      case of `match`, we use `okToFloat` to check that it does not mention any
      locally bound (lambda, case) variables.  If so we fail.

  (4) In the Let case of `match`, we use GHC.Core.Subst.substBind to
      freshen the binding (which, remember (3), mentions no locally
      bound variables), in a lexically-scoped way (via rv_fltR in
      MatchEnv).

      The subtle point is that we want an in-scope set for this
      substitution that includes /two/ sets:
      * The in-scope variables at this point, so that we avoid using
        those local names for the floated binding; points (a) and (b) above.
      * All "earlier" floated bindings, so that we avoid using the
        same name for two different floated bindings; point (c) above.

      Because we have to compute the in-scope set here, the in-scope set
      stored in `rv_fltR` is always ignored; we leave it only because it's
      convenient to have `rv_fltR :: Subst` (with an always-ignored `InScopeSet`)
      rather than storing three separate substitutions.

  (5) We apply that freshening substitution, in a lexically-scoped
      way to the term, although lazily; this is the rv_fltR field.

See #4814, which is an issue resulting from getting this wrong.

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
ruleCheckProgram :: RuleOpts                    -- ^ Rule options
                 -> CompilerPhase               -- ^ Rule activation test
                 -> String                      -- ^ Rule pattern
                 -> (Id -> [CoreRule])          -- ^ Rules for an Id
                 -> CoreProgram                 -- ^ Bindings to check in
                 -> SDoc                        -- ^ Resulting check message
ruleCheckProgram ropts phase rule_pat rules binds
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
                       , rc_rules     = rules
                       , rc_ropts     = ropts
                       }
    results = unionManyBags (map (ruleCheckBind env) binds)
    line = text (replicate 20 '-')

data RuleCheckEnv = RuleCheckEnv {
    rc_is_active :: Activation -> Bool,
    rc_id_unf  :: IdUnfoldingFun,
    rc_pattern :: String,
    rc_rules :: Id -> [CoreRule],
    rc_ropts :: RuleOpts
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
                                unionManyBags [ruleCheck env r | Alt _ _ r <- as]

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

    check_rule rule = rule_herald rule <> colon <+> rule_info (rc_ropts env) rule

    rule_herald (BuiltinRule { ru_name = name })
        = text "Builtin rule" <+> doubleQuotes (ftext name)
    rule_herald (Rule { ru_name = name })
        = text "Rule" <+> doubleQuotes (ftext name)

    rule_info opts rule
        | Just _ <- matchRule opts (emptyInScopeSet, rc_id_unf env)
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
          match_fn rule_arg arg = match renv emptyRuleSubst rule_arg arg MRefl
                where
                  in_scope = mkInScopeSet (lhs_fvs `unionVarSet` exprFreeVars arg)
                  renv = RV { rv_lcl   = mkRnEnv2 in_scope
                            , rv_tmpls = mkVarSet rule_bndrs
                            , rv_fltR  = mkEmptySubst in_scope
                            , rv_unf   = rc_id_unf env }

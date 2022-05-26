{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE NamedFieldPuns #-}

-- | The CoreRule type and its friends are dealt with mainly in
-- GHC.Core.Rules, but GHC.Core.FVs, GHC.Core.Subst, GHC.Core.Ppr,
-- GHC.Core.Tidy also inspect the representation.
module GHC.Core.Rules (
        -- * Core rule data types
        CoreRule(..),
        RuleName, RuleFun, IdUnfoldingFun, InScopeEnv, RuleOpts,

        -- ** Operations on 'CoreRule's
        ruleArity, ruleName, ruleIdName, ruleActivation,
        setRuleIdName, ruleModule,
        isBuiltinRule, isLocalRule, isAutoRule,
    ) where

import GHC.Prelude

import GHC.Types.Var.Env( InScopeSet )
import GHC.Types.Var
import GHC.Core
import GHC.Core.Orphans
import GHC.Core.Unfoldings
import GHC.Core.Rules.Config ( RuleOpts )
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Types.Basic

-- | A 'CoreRule' is:
--
-- * \"Local\" if the function it is a rule for is defined in the
--   same module as the rule itself.
--
-- * \"Orphan\" if nothing on the LHS is defined in the same module
--   as the rule itself
data CoreRule
  = Rule {
        ru_name :: RuleName,            -- ^ Name of the rule, for communication with the user
        ru_act  :: Activation,          -- ^ When the rule is active

        -- Rough-matching stuff
        -- see comments with InstEnv.ClsInst( is_cls, is_rough )
        ru_fn    :: !Name,               -- ^ Name of the 'GHC.Types.Id.Id' at the head of this rule
        ru_rough :: [Maybe Name],       -- ^ Name at the head of each argument to the left hand side

        -- Proper-matching stuff
        -- see comments with InstEnv.ClsInst( is_tvs, is_tys )
        ru_bndrs :: [CoreBndr],         -- ^ Variables quantified over
        ru_args  :: [CoreExpr],         -- ^ Left hand side arguments

        -- And the right-hand side
        ru_rhs   :: CoreExpr,           -- ^ Right hand side of the rule
                                        -- Occurrence info is guaranteed correct
                                        -- See Note [OccInfo in unfoldings and rules]

        -- Locality
        ru_auto :: Bool,   -- ^ @True@  <=> this rule is auto-generated
                           --               (notably by Specialise or SpecConstr)
                           --   @False@ <=> generated at the user's behest
                           -- See Note [Trimming auto-rules] in "GHC.Iface.Tidy"
                           -- for the sole purpose of this field.

        ru_origin :: !Module,   -- ^ 'Module' the rule was defined in, used
                                -- to test if we should see an orphan rule.

        ru_orphan :: !IsOrphan, -- ^ Whether or not the rule is an orphan.

        ru_local :: Bool        -- ^ @True@ iff the fn at the head of the rule is
                                -- defined in the same module as the rule
                                -- and is not an implicit 'Id' (like a record selector,
                                -- class operation, or data constructor).  This
                                -- is different from 'ru_orphan', where a rule
                                -- can avoid being an orphan if *any* Name in
                                -- LHS of the rule was defined in the same
                                -- module as the rule.
    }

  -- | Built-in rules are used for constant folding
  -- and suchlike.  They have no free variables.
  -- A built-in rule is always visible (there is no such thing as
  -- an orphan built-in rule.)
  | BuiltinRule {
        ru_name  :: RuleName,   -- ^ As above
        ru_fn    :: Name,       -- ^ As above
        ru_nargs :: Int,        -- ^ Number of arguments that 'ru_try' consumes,
                                -- if it fires, including type arguments
        ru_try   :: RuleFun
                -- ^ This function does the rewrite.  It given too many
                -- arguments, it simply discards them; the returned 'CoreExpr'
                -- is just the rewrite of 'ru_fn' applied to the first 'ru_nargs' args
    }
                -- See Note [Extra args in the target] in GHC.Core.Rules

-- | The 'InScopeSet' in the 'InScopeEnv' is a /superset/ of variables that are
-- currently in scope. See Note [The InScopeSet invariant].
type RuleFun = RuleOpts -> InScopeEnv -> Id -> [CoreExpr] -> Maybe CoreExpr
type InScopeEnv = (InScopeSet, IdUnfoldingFun)

type IdUnfoldingFun = Id -> Unfolding
-- A function that embodies how to unfold an Id if you need
-- to do that in the Rule.  The reason we need to pass this info in
-- is that whether an Id is unfoldable depends on the simplifier phase

isBuiltinRule :: CoreRule -> Bool
isBuiltinRule (BuiltinRule {}) = True
isBuiltinRule _                = False

isAutoRule :: CoreRule -> Bool
isAutoRule (BuiltinRule {}) = False
isAutoRule (Rule { ru_auto = is_auto }) = is_auto

-- | The number of arguments the 'ru_fn' must be applied
-- to before the rule can match on it
ruleArity :: CoreRule -> Int
ruleArity (BuiltinRule {ru_nargs = n}) = n
ruleArity (Rule {ru_args = args})      = length args

ruleName :: CoreRule -> RuleName
ruleName = ru_name

ruleModule :: CoreRule -> Maybe Module
ruleModule Rule { ru_origin } = Just ru_origin
ruleModule BuiltinRule {} = Nothing

ruleActivation :: CoreRule -> Activation
ruleActivation (BuiltinRule { })       = AlwaysActive
ruleActivation (Rule { ru_act = act }) = act

-- | The 'Name' of the 'GHC.Types.Id.Id' at the head of the rule left hand side
ruleIdName :: CoreRule -> Name
ruleIdName = ru_fn

isLocalRule :: CoreRule -> Bool
isLocalRule = ru_local

-- | Set the 'Name' of the 'GHC.Types.Id.Id' at the head of the rule left hand side
setRuleIdName :: Name -> CoreRule -> CoreRule
setRuleIdName nm ru = ru { ru_fn = nm }

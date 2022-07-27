{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module GHC.Driver.Core.Rules ( readRuleEnv ) where

import GHC.Prelude

import GHC.Driver.Env

import GHC.Core
import GHC.Core.Rules   ( extendRuleBaseList, extendRuleEnv, mkRuleBase )

import GHC.Unit.External ( ExternalPackageState(..) )
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.Deps

import GHC.Unit.Module

-- FIXME(Ericson3214): factor out another function which combines EPS
-- and HPT but doesn't include the current modules itself, it would take
-- a 'Module' but not whole 'ModGuts' for this purpose.
--
-- I would do right away, but that changes the 'extendRuleBaseList'
-- order and I am not sure what the ramifications of that are.

-- | Get all the rules
--
-- In particular, get them from
--
-- 1. The current module (as specified by the 'ModGuts').
--
-- 2. Home packages (in memory in this GHC sesssion).
--
-- 3. External packages (on disk, compiled in another GHC session).
--
-- This is useful to separate the infrastructure details of "external vs
-- home" from domain-specific logic that just wants rules and doesn't
-- care where they come from.
readRuleEnv :: HscEnv -> ModGuts -> IO RuleEnv
readRuleEnv hsc_env guts = extendRuleEnv base_ruleenv <$> read_eps_rules
  where
    this_mod = mg_module guts
    gwib = GWIB { gwib_mod = moduleName this_mod, gwib_isBoot = NotBoot }
    hpt_rule_base = mkRuleBase (hptRules hsc_env (moduleUnitId this_mod) gwib)
    -- Forcing this value to avoid unnessecary allocations.
    -- Not doing so results in +25.6% allocations of LargeRecord.
    !rule_base = extendRuleBaseList hpt_rule_base (mg_rules guts)
    vis_orphs = this_mod : dep_orphs (mg_deps guts)
    base_ruleenv = mkRuleEnv rule_base vis_orphs
    read_eps_rules = eps_rule_base <$> hscEPS hsc_env

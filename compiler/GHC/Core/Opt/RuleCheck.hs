-- | Check for non-application of rules matching a pattern
module GHC.Core.Opt.RuleCheck ( RuleCheckOpts (..), ruleCheckPass ) where

import GHC.Prelude

import GHC.Core         ( RuleBase, RuleEnv(..), RuleOpts )
import GHC.Core.Rules   ( ruleCheckProgram, getRules )

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable

import GHC.Unit.Module.Env
import GHC.Unit.Module.ModGuts

import GHC.Types.Basic

data RuleCheckOpts = RuleCheckOpts
  { rc_phase :: !CompilerPhase
  , rc_pattern :: !String
  , rc_rule_opts :: !RuleOpts
  }

ruleCheckPass :: Logger
              -> RuleCheckOpts
              -> RuleBase
              -> ModuleSet
              -> ModGuts
              -> IO ModGuts
ruleCheckPass logger opts rb vis_orphs guts =
  withTiming logger (text "RuleCheck"<+>brackets (ppr $ mg_module guts))
            (const ()) $ do
    let rule_fn fn = getRules (RuleEnv [rb] vis_orphs) fn
                      ++ (mg_rules guts)
    logDumpMsg logger "Rule check" $
        ruleCheckProgram ropts current_phase pat rule_fn (mg_binds guts)
    return guts
  where
    current_phase = rc_phase opts
    pat = rc_pattern opts
    ropts = rc_rule_opts opts

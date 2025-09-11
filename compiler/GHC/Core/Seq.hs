-- |
-- Various utilities for forcing Core structures
--
-- It can often be useful to force various parts of the AST. This module
-- provides a number of @seq@-like functions to accomplish this.

module GHC.Core.Seq (
        -- * Utilities for forcing Core structures
        seqExpr, seqExprs, seqUnfolding, seqRules,
        megaSeqIdInfo, seqRuleInfo, seqBinds,
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Types.Id.Info
import GHC.Types.Demand( seqDemand, seqDmdSig )
import GHC.Types.Cpr( seqCprSig )
import GHC.Types.Basic( seqOccInfo )
import GHC.Types.Tickish
import GHC.Types.Var( varType, tyVarKind )
import GHC.Core.Type( seqType, isTyVar )
import GHC.Core.Coercion( seqCo )
import GHC.Types.Id( idInfo )

-- | Evaluate all the fields of the 'IdInfo' that are generally demanded by the
-- compiler
megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqRuleInfo (ruleInfo info)                 `seq`

-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all
--    seqUnfolding (realUnfoldingInfo info)         `seq`

    seqDemand (demandInfo info)                 `seq`
    seqDmdSig (dmdSigInfo info)          `seq`
    seqCprSig (cprSigInfo info)                    `seq`
    seqCaf (cafInfo info)                       `seq`
    seqOneShot (oneShotInfo info)               `seq`
    seqOccInfo (occInfo info)

seqOneShot :: OneShotInfo -> ()
seqOneShot l = l `seq` ()

seqRuleInfo :: RuleInfo -> ()
seqRuleInfo (RuleInfo rules) = seqRules rules

seqCaf :: CafInfo -> ()
seqCaf c = c `seq` ()

seqRules :: [CoreRule] -> ()
seqRules [] = ()
seqRules (Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs } : rules)
  = seqBndrs bndrs `seq` seqExprs (rhs:args) `seq` seqRules rules
seqRules (BuiltinRule {} : rules) = seqRules rules

seqExpr :: CoreExpr -> ()
seqExpr (Var v)         = v `seq` ()
seqExpr (Lit lit)       = lit `seq` ()
seqExpr (App f a)       = seqExpr f `seq` seqExpr a
seqExpr (Lam b e)       = seqBndr b `seq` seqExpr e
seqExpr (Let b e)       = seqBind b `seq` seqExpr e
seqExpr (Case e b t as) = seqExpr e `seq` seqBndr b `seq` seqType t `seq` seqAlts as
seqExpr (Cast e co)     = seqExpr e `seq` seqCo co
seqExpr (Tick n e)      = seqTickish n `seq` seqExpr e
seqExpr (Type t)        = seqType t
seqExpr (Coercion co)   = seqCo co

seqExprs :: [CoreExpr] -> ()
seqExprs [] = ()
seqExprs (e:es) = seqExpr e `seq` seqExprs es

seqTickish :: CoreTickish -> ()
seqTickish ProfNote{ profNoteCC = cc } = cc `seq` ()
seqTickish HpcTick{} = ()
seqTickish Breakpoint{ breakpointFVs = ids } = seqBndrs ids
seqTickish SourceNote{} = ()

seqBndr :: CoreBndr -> ()
seqBndr b | isTyVar b = seqType (tyVarKind b)
          | otherwise = seqType (varType b)             `seq`
                        megaSeqIdInfo (idInfo b)

seqBndrs :: [CoreBndr] -> ()
seqBndrs [] = ()
seqBndrs (b:bs) = seqBndr b `seq` seqBndrs bs

seqBinds :: [Bind CoreBndr] -> ()
seqBinds bs = foldr (seq . seqBind) () bs

seqBind :: Bind CoreBndr -> ()
seqBind (NonRec b e) = seqBndr b `seq` seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs :: [(CoreBndr, CoreExpr)] -> ()
seqPairs [] = ()
seqPairs ((b,e):prs) = seqBndr b `seq` seqExpr e `seq` seqPairs prs

seqAlts :: [CoreAlt] -> ()
seqAlts [] = ()
seqAlts (Alt c bs e:alts) = c `seq` seqBndrs bs `seq` seqExpr e `seq` seqAlts alts

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding { uf_tmpl = e, uf_is_top = top,
                uf_cache = cache, uf_guidance = g})
  = seqExpr e `seq` top `seq` cache `seq` seqGuidance g
    -- The unf_cache :: UnfoldingCache field is a strict data type,
    -- so it is sufficient to use plain `seq` for this field
    -- See Note [UnfoldingCache] in GHC.Core

seqUnfolding _ = ()

seqGuidance :: UnfoldingGuidance -> ()
seqGuidance (UnfIfGoodArgs ns n b) = n `seq` sum ns `seq` b `seq` ()
seqGuidance _                      = ()

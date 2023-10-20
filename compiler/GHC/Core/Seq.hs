-- |
-- Various utilities for forcing Core structures
--
-- It can often be useful to force various parts of the AST. This module
-- provides a number of @seq@-like functions to accomplish this.

module GHC.Core.Seq (
        -- * Utilities for forcing Core structures
        seqExpr, seqExprs, seqUnfolding, seqRules,
        megaSeqIdInfo, seqRuleInfo, seqBinds, seqGuidance
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Type( seqType, isTyVar )
import GHC.Core.Coercion( seqCo )

import GHC.Types.Id.Info
import GHC.Types.Demand( seqDemand, seqDmdSig )
import GHC.Types.Cpr( seqCprSig )
import GHC.Types.Basic( seqOccInfo )
import GHC.Types.Tickish
import GHC.Types.Var.Set( seqDVarSet )
import GHC.Types.Var( varType, tyVarKind )
import GHC.Types.Id( idInfo )

import GHC.Data.Bag( Bag )

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
seqRuleInfo (RuleInfo rules fvs) = seqRules rules `seq` seqDVarSet fvs

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
seqExprs = seqList seqExpr

seqBag :: (a -> ()) -> Bag a -> ()
seqBag f = foldr (seq . f) ()

seqList :: (a -> ()) -> [a] -> ()
seqList f = foldr (seq . f) ()

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
seqBndrs = seqList seqBndr

seqBinds :: [Bind CoreBndr] -> ()
seqBinds = seqList seqBind

seqBind :: Bind CoreBndr -> ()
seqBind (NonRec b e) = seqBndr b `seq` seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs :: [(CoreBndr, CoreExpr)] -> ()
seqPairs [] = ()
seqPairs ((b,e):prs) = seqBndr b `seq` seqExpr e `seq` seqPairs prs

seqAlts :: [CoreAlt] -> ()
seqAlts = seqList seqAlt

seqAlt :: CoreAlt -> ()
seqAlt (Alt c bs e) = c `seq` seqBndrs bs `seq` seqExpr e

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding { uf_tmpl = e, uf_is_top = top
                            , uf_cache = cache, uf_guidance = g})
  = seqExpr e `seq` top `seq` cache `seq` seqGuidance g
    -- The unf_cache :: UnfoldingCache field is a strict data type,
    -- so it is sufficient to use plain `seq` for this field
    -- See Note [UnfoldingCache] in GHC.Core

seqUnfolding _ = ()

seqGuidance :: UnfoldingGuidance -> ()
seqGuidance (UnfIfGoodArgs bs et) = seqBndrs bs `seq` seqET et
seqGuidance _                     = ()

seqET :: ExprTree -> ()
seqET (ExprTree { et_wc_tot = tot, et_size = size, et_cases = cases, et_ret = ret })
  = tot `seq` size `seq` ret `seq` seqBag seqCT cases

seqCT :: CaseTree -> ()
seqCT (ScrutOf x i) = x `seq` i `seq` ()
seqCT (CaseOf x y alts) = x `seq` y `seq` seqList seqAT alts

seqAT :: AltTree -> ()
seqAT (AltTree con bs e) = con `seq` seqBndrs bs `seq` seqET e


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
import GHC.Types.Basic( seqOccInfo, SeqResult, seqUnit )
import GHC.Types.Tickish
import GHC.Types.Var.Set( seqDVarSet )
import GHC.Types.Var( varType, tyVarKind )
import GHC.Core.Type( seqType, isTyVar )
import GHC.Core.Coercion( seqCo )
import GHC.Types.Id( idInfo )
import qualified Data.Monoid as M

-- | Evaluate all the fields of the 'IdInfo' that are generally demanded by the
-- compiler
megaSeqIdInfo :: IdInfo -> SeqResult
megaSeqIdInfo info
  = seqRuleInfo (ruleInfo info)         M.<>

-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all
--    seqUnfolding (realUnfoldingInfo info)         M.<>

    seqDemand (demandInfo info)         M.<>
    seqDmdSig (dmdSigInfo info)         M.<>
    seqCprSig (cprSigInfo info)         M.<>
    seqCaf (cafInfo info)               M.<>
    seqOneShot (oneShotInfo info)       M.<>
    seqOccInfo (occInfo info)

seqOneShot :: OneShotInfo -> SeqResult
seqOneShot = seqUnit

seqRuleInfo :: RuleInfo -> SeqResult
seqRuleInfo (RuleInfo rules fvs) = seqRules rules M.<> seqDVarSet fvs

seqCaf :: CafInfo -> SeqResult
seqCaf = seqUnit

seqRules :: [CoreRule] -> SeqResult
seqRules [] = mempty
seqRules (Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs } : rules)
  = seqBndrs bndrs M.<> seqExprs (rhs:args) M.<> seqRules rules
seqRules (BuiltinRule {} : rules) = seqRules rules

seqExpr :: CoreExpr -> SeqResult
seqExpr (Var v)         = seqUnit v
seqExpr (Lit lit)       = seqUnit lit
seqExpr (App f a)       = seqExpr f M.<> seqExpr a
seqExpr (Lam b e)       = seqBndr b M.<> seqExpr e
seqExpr (Let b e)       = seqBind b M.<> seqExpr e
seqExpr (Case e b t as) = seqExpr e M.<> seqBndr b M.<> seqType t M.<> seqAlts as
seqExpr (Cast e co)     = seqExpr e M.<> seqCo co
seqExpr (Tick n e)      = seqTickish n M.<> seqExpr e
seqExpr (Type t)        = seqType t
seqExpr (Coercion co)   = seqCo co

seqExprs :: [CoreExpr] -> SeqResult
seqExprs []     = mempty
seqExprs (e:es) = seqExpr e M.<> seqExprs es

seqTickish :: CoreTickish -> SeqResult
seqTickish ProfNote{ profNoteCC = cc } = seqUnit cc
seqTickish HpcTick{} = mempty
seqTickish Breakpoint{ breakpointFVs = ids } = seqBndrs ids
seqTickish SourceNote{} = mempty

seqBndr :: CoreBndr -> SeqResult
seqBndr b | isTyVar b = seqType (tyVarKind b)
          | otherwise = seqType (varType b)
                        M.<> megaSeqIdInfo (idInfo b)

seqBndrs :: [CoreBndr] -> SeqResult
seqBndrs []     = mempty
seqBndrs (b:bs) = seqBndr b M.<> seqBndrs bs

seqBinds :: [Bind CoreBndr] -> SeqResult
seqBinds bs = foldr ((M.<>) . seqBind) mempty bs

seqBind :: Bind CoreBndr -> SeqResult
seqBind (NonRec b e) = seqBndr b M.<> seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs :: [(CoreBndr, CoreExpr)] -> SeqResult
seqPairs []          = mempty
seqPairs ((b,e):prs) = seqBndr b M.<> seqExpr e M.<> seqPairs prs

seqAlts :: [CoreAlt] -> SeqResult
seqAlts []                = mempty
seqAlts (Alt c bs e:alts) = seqUnit c M.<> seqBndrs bs M.<> seqExpr e M.<> seqAlts alts

seqUnfolding :: Unfolding -> SeqResult
seqUnfolding (CoreUnfolding { uf_tmpl = e, uf_is_top = top
                            , uf_cache = cache, uf_guidance = g})
  = seqExpr e M.<> seqUnit top M.<> seqUnit cache M.<> seqGuidance g
    -- The unf_cache :: UnfoldingCache field is a strict data type,
    -- so it is sufficient to use plain `seq` for this field
    -- See Note [UnfoldingCache] in GHC.Core

seqUnfolding _ = mempty

seqGuidance :: UnfoldingGuidance -> SeqResult
seqGuidance (UnfIfGoodArgs ns n b) = seqUnit n M.<> seqUnit (sum ns) M.<> seqUnit b
seqGuidance _                      = mempty


module PprCmmZ
    ( pprCmmGraphLikeCmm
    )
where

import BlockId
import Cmm
import PprCmm
import Outputable
import qualified ZipCfgCmmRep as G
import qualified ZipCfg as Z
import CmmZipUtil

import Data.Maybe
import FastString

----------------------------------------------------------------
-- | The purpose of this function is to print a Cmm zipper graph "as if it were"
-- a Cmm program.  The objective is dodgy, so it's unsurprising parts of the
-- code are dodgy as well.

pprCmmGraphLikeCmm :: G.CmmGraph -> SDoc
pprCmmGraphLikeCmm g = vcat (swallow blocks)
    where blocks = Z.postorder_dfs g
          swallow :: [G.CmmBlock] -> [SDoc]
          swallow [] = []
          swallow (Z.Block id t : rest) = tail id [] Nothing t rest
          tail id prev' out (Z.ZTail m t) rest = tail id (mid m : prev') out t rest
          tail id prev' out (Z.ZLast (Z.LastOther l)) rest = last id prev' out l rest
          tail id prev' _   (Z.ZLast Z.LastExit)      rest = exit id prev' rest
          mid m = ppr m
          block' id prev'
              | id == Z.lg_entry g, entry_has_no_pred =
                            vcat (text "<entry>" : reverse prev')
              | otherwise = hang (ppr id <> colon) 4 (vcat (reverse prev'))
          last id prev' out l n =
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case l of
                G.LastBranch tgt ->
                    case n of
                      Z.Block id' t : bs
                          | tgt == id', unique_pred id' 
                          -> tail id prev' out t bs  -- optimize out redundant labels
                      _ -> endblock (ppr $ CmmBranch tgt)
                l@(G.LastCondBranch expr tid fid) ->
                  let ft id = text "// fall through to " <> ppr id in
                  case n of
                    Z.Block id' t : bs
                      | id' == fid, isNothing out ->
                          tail id (ft fid : ppr (CmmCondBranch expr tid) : prev') Nothing t bs
                      | id' == tid, Just e' <- maybeInvertCmmExpr expr, isNothing out->
                          tail id (ft tid : ppr (CmmCondBranch e'   fid) : prev') Nothing t bs
                    _ -> endblock $ with_out out l
                l@(G.LastSwitch {})      -> endblock $ with_out out l
                l@(G.LastCall _ _ _ _ _) -> endblock $ with_out out l
          exit id prev' n = -- highly irregular (assertion violation?)
              let endblock stmt = block' id (stmt : prev') : swallow n in
              endblock (text "// <exit>")
          preds = zipPreds g
          entry_has_no_pred = case lookupBlockEnv preds (Z.lg_entry g) of
                                Nothing -> True
                                Just s -> isEmptyBlockSet s
          single_preds =
              let add b single =
                    let id = Z.blockId b
                    in  case lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeBlockSet s == 1 then
                                        extendBlockSet single id
                                    else single
              in  Z.fold_blocks add emptyBlockSet g
          unique_pred id = elemBlockSet id single_preds

with_out :: Maybe (G.Convention, CmmActuals) -> G.Last -> SDoc
with_out Nothing l = ptext (sLit "??no-arguments??") <+> ppr l
with_out (Just (conv, args)) l = last l
    where last (G.LastCall e k _ _ _) =
              hcat [ptext (sLit "... = foreign "),
                    doubleQuotes(ppr conv), space,
                    ppr_target e, parens ( commafy $ map ppr args ),
                    ptext (sLit " \"safe\""),
                    text " returns to " <+> ppr k,
                    semi ]
          last l = ppr l
          ppr_target (CmmLit lit) = pprLit lit
          ppr_target fn'          = parens (ppr fn')
          commafy xs = hsep $ punctuate comma xs

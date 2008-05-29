
module PprCmmZ
    ( pprCmmGraphLikeCmm
    )
where

import BlockId
import Cmm
import CmmExpr
import ForeignCall
import PprCmm
import Outputable
import qualified ZipCfgCmmRep as G
import qualified ZipCfg as Z
import CmmZipUtil

import Maybe
import UniqSet
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
          tail id prev' out (Z.ZTail (G.CopyOut conv args) t) rest =
              if isJust out then panic "multiple CopyOut nodes in one basic block"
              else
                  tail id (prev') (Just (conv, args)) t rest
          tail id prev' out (Z.ZTail m t) rest = tail id (mid m : prev') out t rest
          tail id prev' out (Z.ZLast Z.LastExit)      rest = exit id prev' out rest
          tail id prev' out (Z.ZLast (Z.LastOther l)) rest = last id prev' out l rest
          mid (G.CopyIn _ [] _) = text "// proc point (no parameters)"
          mid m@(G.CopyIn {}) = ppr m <+> text "(proc point)"
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
                l@(G.LastJump   {}) -> endblock $ with_out out l
                l@(G.LastReturn {}) -> endblock $ with_out out l
                l@(G.LastSwitch {}) -> endblock $ with_out out l
                l@(G.LastCall _ Nothing) -> endblock $ with_out out l
                l@(G.LastCall tgt (Just k))
                   | Z.Block id' (Z.ZTail (G.CopyIn _ ress srt) t) : bs <- n,
                     Just (conv, args) <- out,
                     id' == k ->
                         let call = CmmCall tgt' ress args (CmmSafe srt) CmmMayReturn
                             tgt' = CmmCallee tgt (cconv_of_conv conv)
                             ppcall = ppr call <+> parens (text "ret to" <+> ppr k)
                         in if unique_pred k then
                                tail id (ppcall : prev') Nothing t bs
                            else
                                endblock (ppcall)
                   | Z.Block id' t : bs <- n, id' == k, unique_pred k,
                     Just (conv, args) <- out,
                     Just (ress, srt) <- findCopyIn t ->
                         let call = CmmCall tgt' ress args (CmmSafe srt) CmmMayReturn
                             tgt' = CmmCallee tgt (cconv_of_conv conv)
                             delayed =
                                 ptext (sLit "// delayed CopyIn follows previous call")
                         in  tail id (delayed : ppr call : prev') Nothing t bs
                   | otherwise -> endblock $ with_out out l
          findCopyIn (Z.ZTail (G.CopyIn _ ress srt) _) = Just (ress, srt)
          findCopyIn (Z.ZTail _ t) = findCopyIn t
          findCopyIn (Z.ZLast _) = Nothing
          exit id prev' out n = -- highly irregular (assertion violation?)
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case out of Nothing -> endblock (text "// <exit>")
                          Just (conv, args) -> endblock (ppr (G.CopyOut conv args) $$
                                                         text "// <exit>")
          preds = zipPreds g
          entry_has_no_pred = case lookupBlockEnv preds (Z.lg_entry g) of
                                Nothing -> True
                                Just s -> isEmptyUniqSet s
          single_preds =
              let add b single =
                    let id = Z.blockId b
                    in  case lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeUniqSet s == 1 then
                                        extendBlockSet single id
                                    else single
              in  Z.fold_blocks add emptyBlockSet g
          unique_pred id = elemBlockSet id single_preds
          cconv_of_conv (G.ConventionStandard conv _) = conv
          cconv_of_conv (G.ConventionPrivate {}) = CmmCallConv -- XXX totally bogus

with_out :: Maybe (G.Convention, CmmActuals) -> G.Last -> SDoc
with_out Nothing l = ptext (sLit "??no-arguments??") <+> ppr l
with_out (Just (conv, args)) l = last l
    where last (G.LastCall e k) =
              hcat [ptext (sLit "... = foreign "),
                    doubleQuotes(ppr conv), space,
                    ppr_target e, parens ( commafy $ map ppr args ),
                    ptext (sLit " \"safe\""),
                    case k of Nothing -> ptext (sLit " never returns")
                              Just _ -> empty,
                    semi ]
          last (G.LastReturn) = ppr (CmmReturn args)
          last (G.LastJump e) = ppr (CmmJump e args)
          last l = ppr (G.CopyOut conv args) $$ ppr l
          ppr_target (CmmLit lit) = pprLit lit
          ppr_target fn'          = parens (ppr fn')
          commafy xs = hsep $ punctuate comma xs

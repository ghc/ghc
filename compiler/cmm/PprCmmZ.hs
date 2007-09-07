{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module PprCmmZ 
    ( pprCmmGraph
    )
where

#include "HsVersions.h"

import Cmm
import CmmExpr
import PprCmm()
import Outputable
import qualified ZipCfgCmm as G
import qualified ZipCfg as Z
import qualified ZipDataflow as DF
import CmmZipUtil

import UniqSet
import FastString

----------------------------------------------------------------
instance DF.DebugNodes G.Middle G.Last


instance Outputable G.CmmGraph where
    ppr = pprCmmGraph

pprCmmGraph :: G.CmmGraph -> SDoc
pprCmmGraph g = vcat (swallow blocks)
    where blocks = Z.postorder_dfs g
          swallow :: [G.CmmBlock] -> [SDoc]
          swallow [] = []
          swallow (Z.Block id t : rest) = tail id [] t rest
          tail id prev' (Z.ZTail m t)            rest = tail id (mid m : prev') t rest
          tail id prev' (Z.ZLast Z.LastExit)     rest = exit id prev' rest
          tail id prev' (Z.ZLast (Z.LastOther l))rest = last id prev' l rest
          mid (G.CopyIn _ [] _) = text "// proc point (no parameters)"
          mid m@(G.CopyIn {}) = ppr m <+> text "(proc point)"
          mid m = ppr m
          block' id prev'
              | id == Z.gr_entry g, entry_has_no_pred =
                            vcat (text "<entry>" : reverse prev')
              | otherwise = hang (ppr id <> colon) 4 (vcat (reverse prev'))
          last id prev' l n =
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case l of
                G.LastBranch tgt [] ->
                    case n of
                      Z.Block id' t : bs
                          | tgt == id', unique_pred id' 
                          -> tail id prev' t bs  -- optimize out redundant labels
                      _ -> endblock (ppr $ CmmBranch tgt)
                l@(G.LastBranch {}) -> endblock (ppr l)
                l@(G.LastCondBranch expr tid fid) ->
                  let ft id = text "// fall through to " <> ppr id in
                  case n of
                    Z.Block id' t : bs
                      | id' == fid, False ->
                          tail id (ft fid : ppr (CmmCondBranch expr tid) : prev') t bs
                      | id' == tid, Just e' <- maybeInvertCmmExpr expr, False ->
                          tail id (ft tid : ppr (CmmCondBranch e'   fid) : prev') t bs
                    _ -> endblock (ppr l)
                l@(G.LastJump   {}) -> endblock $ ppr l
                l@(G.LastReturn {}) -> endblock $ ppr l
                l@(G.LastSwitch {}) -> endblock $ ppr l
                l@(G.LastCall _ _ Nothing) -> endblock $ ppr l
                l@(G.LastCall tgt args (Just k))
                   | Z.Block id' (Z.ZTail (G.CopyIn _ ress srt) t) : bs <- n,
                     id' == k ->
                         let call = CmmCall tgt ress args (CmmSafe srt) CmmMayReturn
                             ppcall = ppr call <+> parens (text "ret to" <+> ppr k)
                         in if unique_pred k then
                                tail id (ppcall : prev') t bs
                            else
                                endblock (ppcall)
                   | Z.Block id' t : bs <- n, id' == k, unique_pred k,
                     Just (ress, srt) <- findCopyIn t ->
                         let call = CmmCall tgt ress args (CmmSafe srt) CmmMayReturn
                             delayed =
                                 ptext SLIT("// delayed CopyIn follows previous call")
                         in  tail id (delayed : ppr call : prev') t bs
                   | otherwise -> endblock $ ppr l
          findCopyIn (Z.ZTail (G.CopyIn _ ress srt) _) = Just (ress, srt)
          findCopyIn (Z.ZTail _ t) = findCopyIn t
          findCopyIn (Z.ZLast _) = Nothing
          exit id prev' n = -- highly irregular (assertion violation?)
              let endblock stmt = block' id (stmt : prev') : swallow n in
              endblock (text "// <exit>")
{-
              case n of [] -> [text "<exit>"]
                        Z.Block id' t : bs -> 
                            if unique_pred id' then
                                tail id (ptext SLIT("went thru exit") : prev') t bs 
                            else
                                endblock (ppr $ CmmBranch id')
-}
          preds = zipPreds g
          entry_has_no_pred = case Z.lookupBlockEnv preds (Z.gr_entry g) of
                                Nothing -> True
                                Just s -> isEmptyUniqSet s
          single_preds =
              let add b single =
                    let id = Z.blockId b
                    in  case Z.lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeUniqSet s == 1 then
                                        Z.extendBlockSet single id
                                    else single
              in  Z.fold_blocks add Z.emptyBlockSet g
          unique_pred id = Z.elemBlockSet id single_preds


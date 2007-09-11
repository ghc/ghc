{-# LANGUAGE PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module CmmCvt
  ( cmmToZgraph, cmmOfZgraph )
where
import Cmm
import CmmExpr
import MkZipCfgCmm hiding (CmmGraph)
import ZipCfgCmmRep -- imported for reverse conversion
import CmmZipUtil
import FastString
import Outputable
import Panic
import PprCmm()
import PprCmmZ()
import UniqSet
import UniqSupply
import qualified ZipCfg as G

cmmToZgraph :: GenCmm d h (ListGraph CmmStmt) -> UniqSM (GenCmm d h CmmGraph)
cmmOfZgraph :: GenCmm d h (CmmGraph)          ->         GenCmm d h (ListGraph CmmStmt)

cmmToZgraph = cmmMapGraphM toZgraph
cmmOfZgraph = cmmMapGraph  ofZgraph


toZgraph :: String -> ListGraph CmmStmt -> UniqSM CmmGraph
toZgraph _ (ListGraph []) = lgraphOfAGraph emptyAGraph
toZgraph fun_name g@(ListGraph (BasicBlock id ss : other_blocks)) = 
           labelAGraph id $ mkStmts ss <*> foldr addBlock emptyAGraph other_blocks
  where addBlock (BasicBlock id ss) g = mkLabel id   <*> mkStmts ss <*> g
        mkStmts (CmmNop        : ss)  = mkNop        <*> mkStmts ss 
        mkStmts (CmmComment s  : ss)  = mkComment s  <*> mkStmts ss
        mkStmts (CmmAssign l r : ss)  = mkAssign l r <*> mkStmts ss
        mkStmts (CmmStore  l r : ss)  = mkStore  l r <*> mkStmts ss
        mkStmts (CmmCall f res args (CmmSafe srt) CmmMayReturn : ss) =
                      mkCall       f res args srt <*> mkStmts ss 
        mkStmts (CmmCall f res args CmmUnsafe CmmMayReturn : ss) =
                      mkUnsafeCall f res args     <*> mkStmts ss
        mkStmts (CmmCondBranch e l : fbranch) =
            mkCmmIfThenElse e (mkBranch l) (mkStmts fbranch)
        mkStmts (last : []) = mkLast last
        mkStmts []          = bad "fell off end"
        mkStmts (_ : _ : _) = bad "last node not at end"
        bad msg = pprPanic (msg ++ " in function " ++ fun_name) (ppr g)
        mkLast (CmmCall f  []     args _ CmmNeverReturns) = mkFinalCall f args
        mkLast (CmmSwitch scrutinee table) = mkSwitch scrutinee table
        mkLast (CmmJump tgt args)          = mkJump tgt args
        mkLast (CmmReturn ress)            = mkReturn ress
        mkLast (CmmBranch tgt)             = mkBranch tgt
        mkLast (CmmCall _f (_:_) _args _ CmmNeverReturns) =
                   panic "Call never returns but has results?!"
        mkLast _ = panic "fell off end of block"

ofZgraph :: CmmGraph -> ListGraph CmmStmt
ofZgraph g = ListGraph $ swallow blocks
    where blocks = G.postorder_dfs g
          -- | the next two functions are hooks on which to hang debugging info
          extend_entry stmts = stmts
          extend_block _id stmts = stmts
          _extend_entry stmts = scomment showblocks : scomment cscomm : stmts
          showblocks = "LGraph has " ++ show (length blocks) ++ " blocks:" ++
                       concat (map (\(G.Block id _) -> " " ++ show id) blocks)
          cscomm = "Call successors are" ++
                   (concat $ map (\id -> " " ++ show id) $ uniqSetToList call_succs)
          swallow [] = []
          swallow (G.Block id t : rest) = tail id [] t rest
          tail id prev' (G.ZTail m t)            rest = tail id (mid m : prev') t rest
          tail id prev' (G.ZLast G.LastExit)     rest = exit id prev' rest
          tail id prev' (G.ZLast (G.LastOther l))rest = last id prev' l rest
          mid (MidNop)        = CmmNop
          mid (MidComment s)  = CmmComment s
          mid (MidAssign l r) = CmmAssign l r
          mid (MidStore  l r) = CmmStore  l r
          mid (MidUnsafeCall f ress args) = CmmCall f ress args CmmUnsafe CmmMayReturn
          mid m@(CopyOut {})  = pcomment (ppr m)
          mid m@(CopyIn {})   = pcomment (ppr m <+> text "(proc point)")
          pcomment p = scomment $ showSDoc p
          block' id prev'
              | id == G.gr_entry g = BasicBlock id $ extend_entry    (reverse prev')
              | otherwise          = BasicBlock id $ extend_block id (reverse prev')
          last id prev' l n =
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case l of
                LastBranch _ (_:_) -> panic "unrepresentable branch"
                LastBranch tgt [] ->
                    case n of
                      G.Block id' t : bs
                          | tgt == id', unique_pred id' 
                          -> tail id prev' t bs  -- optimize out redundant labels
                      _ -> endblock (CmmBranch tgt)
                LastCondBranch expr tid fid ->
                  case n of
                    G.Block id' t : bs
                      | id' == fid, unique_pred id' ->
                                      tail id (CmmCondBranch expr tid : prev') t bs
                      | id' == tid, unique_pred id',
                        Just e' <- maybeInvertCmmExpr expr ->
                                      tail id (CmmCondBranch e'   fid : prev') t bs
                    _ -> let instrs' = CmmBranch fid : CmmCondBranch expr tid : prev'
                         in block' id instrs' : swallow n
                LastJump expr params -> endblock $ CmmJump expr params 
                LastReturn params    -> endblock $ CmmReturn params
                LastSwitch arg ids   -> endblock $ CmmSwitch arg $ ids
                LastCall tgt args Nothing ->
                    endblock $ CmmCall tgt [] args CmmUnsafe CmmNeverReturns
                LastCall tgt args (Just k)
                   | G.Block id' (G.ZTail (CopyIn _ ress srt) t) : bs <- n,
                     id' == k, unique_pred k ->
                         let call = CmmCall tgt ress args (CmmSafe srt) CmmMayReturn
                         in  tail id (call : prev') t bs
                   | G.Block id' t : bs <- n, id' == k, unique_pred k ->
                         let (ress, srt) = findCopyIn t
                             call = CmmCall tgt ress args (CmmSafe srt) CmmMayReturn
                             delayed = scomment "delayed CopyIn follows previous call"
                         in  tail id (delayed : call : prev') t bs
                   | otherwise -> panic "unrepairable call"
          findCopyIn (G.ZTail (CopyIn _ ress srt) _) = (ress, srt)
          findCopyIn (G.ZTail _ t) = findCopyIn t
          findCopyIn (G.ZLast _) = panic "missing CopyIn after call"
          exit id prev' n = -- highly irregular (assertion violation?)
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case n of [] -> endblock (scomment "procedure falls off end")
                        G.Block id' t : bs -> 
                            if unique_pred id' then
                                tail id (scomment "went thru exit" : prev') t bs 
                            else
                                endblock (CmmBranch id')
          preds = zipPreds g
          single_preds =
              let add b single =
                    let id = G.blockId b
                    in  case G.lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeUniqSet s == 1 then
                                        G.extendBlockSet single id
                                    else single
              in  G.fold_blocks add G.emptyBlockSet g
          unique_pred id = G.elemBlockSet id single_preds
          call_succs = 
              let add b succs =
                      case G.last (G.unzip b) of
                        G.LastOther (LastCall _ _ (Just id)) -> extendBlockSet succs id
                        _ -> succs
              in  G.fold_blocks add emptyBlockSet g
          _is_call_succ id = elemBlockSet id call_succs

scomment :: String -> CmmStmt
scomment s = CmmComment $ mkFastString s

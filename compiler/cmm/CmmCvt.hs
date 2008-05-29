{-# LANGUAGE PatternGuards #-}

module CmmCvt
  ( cmmToZgraph, cmmOfZgraph )
where

import BlockId
import Cmm
import CmmExpr
import MkZipCfg
import MkZipCfgCmm hiding (CmmGraph)
import ZipCfgCmmRep -- imported for reverse conversion
import CmmZipUtil
import PprCmm()
import PprCmmZ()
import qualified ZipCfg as G

import FastString
import Monad
import Outputable
import Panic
import UniqSet
import UniqSupply

import Maybe

cmmToZgraph :: GenCmm d h (ListGraph CmmStmt) -> UniqSM (GenCmm d h CmmGraph)
cmmOfZgraph :: GenCmm d h (CmmGraph)          ->         GenCmm d h (ListGraph CmmStmt)

cmmToZgraph (Cmm tops) = liftM Cmm $ mapM mapTop tops
  where mapTop (CmmProc h l args g) =
          toZgraph (showSDoc $ ppr l) args g >>= return . CmmProc h l args
        mapTop (CmmData s ds) = return $ CmmData s ds
cmmOfZgraph = cmmMapGraph  ofZgraph


toZgraph :: String -> CmmFormalsWithoutKinds -> ListGraph CmmStmt -> UniqSM CmmGraph
toZgraph _ _ (ListGraph []) = lgraphOfAGraph emptyAGraph
toZgraph fun_name args g@(ListGraph (BasicBlock id ss : other_blocks)) = 
           labelAGraph id $ mkMiddles (mkEntry area undefined args) <*>
                            mkStmts ss <*> foldr addBlock emptyAGraph other_blocks
  where addBlock (BasicBlock id ss) g = mkLabel id   <*> mkStmts ss <*> g
        mkStmts (CmmNop        : ss)  = mkNop        <*> mkStmts ss 
        mkStmts (CmmComment s  : ss)  = mkComment s  <*> mkStmts ss
        mkStmts (CmmAssign l r : ss)  = mkAssign l r <*> mkStmts ss
        mkStmts (CmmStore  l r : ss)  = mkStore  l r <*> mkStmts ss
        mkStmts (CmmCall (CmmCallee f conv) res args (CmmSafe srt) CmmMayReturn : ss) =
                      mkCall       f conv res args srt <*> mkStmts ss 
        mkStmts (CmmCall (CmmPrim {}) _ _ (CmmSafe _) _ : _) =
            panic "safe call to a primitive CmmPrim CallishMachOp"
        mkStmts (CmmCall f res args CmmUnsafe CmmMayReturn : ss) =
                      mkUnsafeCall f res args     <*> mkStmts ss
        mkStmts (CmmCondBranch e l : fbranch) =
            mkCmmIfThenElse e (mkBranch l) (mkStmts fbranch)
        mkStmts (last : []) = mkLast last
        mkStmts []          = bad "fell off end"
        mkStmts (_ : _ : _) = bad "last node not at end"
        bad msg = pprPanic (msg ++ " in function " ++ fun_name) (ppr g)
        mkLast (CmmCall (CmmCallee f conv) []     args _ CmmNeverReturns) =
            mkFinalCall f conv args
        mkLast (CmmCall (CmmPrim {}) _ _ _ CmmNeverReturns) =
            panic "Call to CmmPrim never returns?!"
        mkLast (CmmSwitch scrutinee table) = mkSwitch scrutinee table
        mkLast (CmmJump tgt args)          = mkJump   area tgt args
        mkLast (CmmReturn ress)            = mkReturn area ress
        mkLast (CmmBranch tgt)             = mkBranch tgt
        mkLast (CmmCall _f (_:_) _args _ CmmNeverReturns) =
                   panic "Call never returns but has results?!"
        mkLast _ = panic "fell off end of block"
        -- The entry, jump, and return areas should be the same.
        -- This code is horrible, but there's no point trying to fix it until we've figured
        -- out our interface for calling conventions.
        -- All return statements are required to use return areas of equal size.
        -- This isn't necessarily required to write correct programs, but it's sane.
        area = case foldr retBlock (retStmts ss Nothing) other_blocks of
                 Just (as, _)  -> mkCallArea id as $ Just args
                 Nothing       -> mkCallArea id [] $ Just args
        retBlock (BasicBlock _ ss) z = retStmts ss z
        retStmts [CmmReturn ress] z@(Just (_, n)) =
          if size ress == n then z
          else panic "return statements in C-- procs must return the same results"
        retStmts [CmmReturn ress] Nothing  = Just (ress, size ress)
        retStmts (_ : rst) z = retStmts rst z
        retStmts [] z = z
        size args = areaSize $ mkCallArea id args Nothing

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
          swallow (G.Block id t : rest) = tail id [] Nothing t rest
          tail id prev' out (G.ZTail (CopyOut conv actuals) t) rest =
              case out of
                Nothing -> tail id prev' (Just (conv, actuals)) t rest
                Just _ -> panic "multiple CopyOut nodes in one basic block"
          tail id prev' out (G.ZTail m t) rest = tail id (mid m : prev') out t rest
          tail id prev' out (G.ZLast G.LastExit)      rest = exit id prev' out rest
          tail id prev' out (G.ZLast (G.LastOther l)) rest = last id prev' out l rest
          mid (MidComment s)  = CmmComment s
          mid (MidAssign l r) = CmmAssign l r
          mid (MidStore  l r) = CmmStore  l r
          mid (MidUnsafeCall f ress args) = CmmCall f ress args CmmUnsafe CmmMayReturn
          mid m@(MidAddToContext {}) = pcomment (ppr m)
          mid m@(CopyOut {})         = pcomment (ppr m)
          mid m@(CopyIn {})          = pcomment (ppr m <+> text "(proc point)")
          pcomment p = scomment $ showSDoc p
          block' id prev'
              | id == G.lg_entry g = BasicBlock id $ extend_entry    (reverse prev')
              | otherwise          = BasicBlock id $ extend_block id (reverse prev')
          last id prev' out l n =
            let endblock stmt = block' id (stmt : prev') : swallow n in
            case l of
              LastBranch tgt ->
                  case n of
                    G.Block id' t : bs
                        | tgt == id', unique_pred id' 
                        -> tail id prev' out t bs -- optimize out redundant labels
                    _ -> if isNothing out then endblock (CmmBranch tgt)
                         else pprPanic "can't convert LGraph with pending CopyOut"
                                  (text "target" <+> ppr tgt <+> ppr g)
              LastCondBranch expr tid fid ->
                if isJust out then pprPanic "CopyOut before conditional branch" (ppr g)
                else
                  case n of
                    G.Block id' t : bs
                      | id' == fid, unique_pred id' ->
                                 tail id (CmmCondBranch expr tid : prev') Nothing t bs
                      | id' == tid, unique_pred id',
                        Just e' <- maybeInvertCmmExpr expr ->
                                 tail id (CmmCondBranch e'   fid : prev') Nothing t bs
                    _ -> let instrs' = CmmBranch fid : CmmCondBranch expr tid : prev'
                         in block' id instrs' : swallow n
              LastJump expr        -> endblock $ with_out out $ CmmJump expr
              LastReturn           -> endblock $ with_out out $ CmmReturn 
              LastSwitch arg ids   -> endblock $ CmmSwitch arg $ ids
              LastCall e cont
                  | Just (conv, args) <- out
                  -> let tgt = CmmCallee e (conv_to_cconv conv) in
                     case cont of
                       Nothing ->
                           endblock $ CmmCall tgt [] args CmmUnsafe CmmNeverReturns
                       Just k
                         | G.Block id' (G.ZTail (CopyIn _ ress srt) t) : bs <- n,
                           id' == k, unique_pred k
                         -> let call = CmmCall tgt ress args (CmmSafe srt) CmmMayReturn
                            in  tail id (call : prev') Nothing t bs
                         | G.Block id' t : bs <- n, id' == k, unique_pred k
                         -> let (ress, srt) = findCopyIn t
                                call = CmmCall tgt ress args (CmmSafe srt) CmmMayReturn
                                delayed = scomment "delayed CopyIn follows prev. call"
                            in  tail id (delayed : call : prev') Nothing t bs
                         | otherwise -> panic "unrepairable call"
                  | otherwise -> panic "call with no CopyOut"
          with_out (Just (_conv, actuals)) f = f actuals
          with_out Nothing f = pprPanic "unrepairable data flow to" (ppr $ f [])
          findCopyIn (G.ZTail (CopyIn _ ress srt) _) = (ress, srt)
          findCopyIn (G.ZTail _ t) = findCopyIn t
          findCopyIn (G.ZLast _) = panic "missing CopyIn after call"
          exit id prev' out n = -- highly irregular (assertion violation?)
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case n of [] -> endblock (scomment "procedure falls off end")
                        G.Block id' t : bs -> 
                            if unique_pred id' then
                                tail id (scomment "went thru exit" : prev') out t bs 
                            else
                                endblock (CmmBranch id')
          conv_to_cconv (ConventionStandard c _) = c
          conv_to_cconv (ConventionPrivate {}) =
              panic "tried to convert private calling convention back to Cmm"
          preds = zipPreds g
          single_preds =
              let add b single =
                    let id = G.blockId b
                    in  case lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeUniqSet s == 1 then
                                        extendBlockSet single id
                                    else single
              in  G.fold_blocks add emptyBlockSet g
          unique_pred id = elemBlockSet id single_preds
          call_succs = 
              let add b succs =
                      case G.last (G.unzip b) of
                        G.LastOther (LastCall _ (Just id)) -> extendBlockSet succs id
                        _ -> succs
              in  G.fold_blocks add emptyBlockSet g
          _is_call_succ id = elemBlockSet id call_succs

scomment :: String -> CmmStmt
scomment s = CmmComment $ mkFastString s

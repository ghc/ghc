{-# LANGUAGE PatternGuards #-}

module CmmCvt
  ( cmmToZgraph, cmmOfZgraph )
where

import BlockId
import Cmm
import MkZipCfgCmm hiding (CmmGraph)
import ZipCfgCmmRep -- imported for reverse conversion
import CmmZipUtil
import PprCmm()
import qualified ZipCfg as G

import FastString
import Control.Monad
import Outputable
import UniqSupply

cmmToZgraph :: GenCmm d h (ListGraph CmmStmt) -> UniqSM (GenCmm d h (CmmStackInfo, CmmGraph))
cmmOfZgraph :: GenCmm d h (CmmStackInfo, CmmGraph)          ->         GenCmm d h (ListGraph CmmStmt)

cmmToZgraph (Cmm tops) = liftM Cmm $ mapM mapTop tops
  where mapTop (CmmProc h l args g) =
          toZgraph (showSDoc $ ppr l) args g >>= return . CmmProc h l args
        mapTop (CmmData s ds) = return $ CmmData s ds
cmmOfZgraph = cmmMapGraph (ofZgraph . snd)

toZgraph :: String -> CmmFormals -> ListGraph CmmStmt -> UniqSM (CmmStackInfo, CmmGraph)
toZgraph _ _ (ListGraph []) =
  do g <- lgraphOfAGraph emptyAGraph
     return ((0, Nothing), g)
toZgraph fun_name args g@(ListGraph (BasicBlock id ss : other_blocks)) = 
           let (offset, entry) = mkEntry id NativeNodeCall args in
           do g <- labelAGraph id $
                     entry <*> mkStmts ss <*> foldr addBlock emptyAGraph other_blocks
              return ((offset, Nothing), g)
  where addBlock (BasicBlock id ss) g =
          mkLabel id <*> mkStmts ss <*> g
        updfr_sz = 0 -- panic "upd frame size lost in cmm conversion"
        mkStmts (CmmNop        : ss)  = mkNop        <*> mkStmts ss 
        mkStmts (CmmComment s  : ss)  = mkComment s  <*> mkStmts ss
        mkStmts (CmmAssign l r : ss)  = mkAssign l r <*> mkStmts ss
        mkStmts (CmmStore  l r : ss)  = mkStore  l r <*> mkStmts ss
        mkStmts (CmmCall (CmmCallee f conv) res args (CmmSafe _) CmmMayReturn : ss) =
            mkCall f (conv', conv') (map hintlessCmm res) (map hintlessCmm args) updfr_sz
            <*> mkStmts ss 
              where conv' = Foreign (ForeignConvention conv [] []) -- JD: DUBIOUS
        mkStmts (CmmCall (CmmPrim {}) _ _ (CmmSafe _) _ : _) =
            panic "safe call to a primitive CmmPrim CallishMachOp"
        mkStmts (CmmCall f res args CmmUnsafe CmmMayReturn : ss) =
                      mkUnsafeCall (convert_target f res args)
			(strip_hints res) (strip_hints args)
                      <*> mkStmts ss
        mkStmts (CmmCondBranch e l : fbranch) =
            mkCmmIfThenElse e (mkBranch l) (mkStmts fbranch)
        mkStmts (last : []) = mkLast last
        mkStmts []          = bad "fell off end"
        mkStmts (_ : _ : _) = bad "last node not at end"
        bad msg = pprPanic (msg ++ " in function " ++ fun_name) (ppr g)
        mkLast (CmmCall (CmmCallee f conv) []     args _ CmmNeverReturns) =
            mkFinalCall f conv (map hintlessCmm args) updfr_sz
        mkLast (CmmCall (CmmPrim {}) _ _ _ CmmNeverReturns) =
            panic "Call to CmmPrim never returns?!"
        mkLast (CmmSwitch scrutinee table) = mkSwitch scrutinee table
        -- SURELY, THESE HINTLESS ARGS ARE WRONG AND WILL BE FIXED WHEN CALLING
        -- CONVENTIONS ARE HONORED?
        mkLast (CmmJump tgt args)          = mkJump   tgt (map hintlessCmm args) updfr_sz
        mkLast (CmmReturn ress)            =
          mkReturnSimple (map hintlessCmm ress) updfr_sz
        mkLast (CmmBranch tgt)             = mkBranch tgt
        mkLast (CmmCall _f (_:_) _args _ CmmNeverReturns) =
                   panic "Call never returns but has results?!"
        mkLast _ = panic "fell off end of block"

strip_hints :: [CmmHinted a] -> [a]
strip_hints = map hintlessCmm

convert_target :: CmmCallTarget -> HintedCmmFormals -> HintedCmmActuals -> MidCallTarget
convert_target (CmmCallee e cc) ress  args  = ForeignTarget e (ForeignConvention cc (map cmmHint args) (map cmmHint ress))
convert_target (CmmPrim op)	   _ress _args = PrimTarget op

add_hints :: Convention -> ValueDirection -> [a] -> [CmmHinted a]
add_hints conv vd args = zipWith CmmHinted args (get_hints conv vd)

get_hints :: Convention -> ValueDirection -> [ForeignHint]
get_hints (Foreign (ForeignConvention _ hints _)) Arguments = hints
get_hints (Foreign (ForeignConvention _ _ hints)) Results   = hints
get_hints _other_conv		  		  _vd       = repeat NoHint

get_conv :: MidCallTarget -> Convention
get_conv (PrimTarget _)       = NativeNodeCall -- JD: SUSPICIOUS
get_conv (ForeignTarget _ fc) = Foreign fc

cmm_target :: MidCallTarget -> CmmCallTarget
cmm_target (PrimTarget op) = CmmPrim op
cmm_target (ForeignTarget e (ForeignConvention cc _ _)) = CmmCallee e cc

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
                   (concat $ map (\id -> " " ++ show id) $ blockSetToList call_succs)
          swallow [] = []
          swallow (G.Block id t : rest) = tail id [] t rest
          tail id prev' (G.ZTail m t)             rest = tail id (mid m : prev') t rest
          tail id prev' (G.ZLast G.LastExit)      rest = exit id prev' rest
          tail id prev' (G.ZLast (G.LastOther l)) rest = last id prev' l rest
          mid (MidComment s)  = CmmComment s
          mid (MidAssign l r) = CmmAssign l r
          mid (MidStore  l r) = CmmStore  l r
          mid (MidForeignCall _ target ress args)
		= CmmCall (cmm_target target)
			  (add_hints conv Results   ress) 
			  (add_hints conv Arguments args) 
			  CmmUnsafe CmmMayReturn
		where
		  conv = get_conv target
          block' id prev'
              | id == G.lg_entry g = BasicBlock id $ extend_entry    (reverse prev')
              | otherwise          = BasicBlock id $ extend_block id (reverse prev')
          last id prev' l n =
            let endblock stmt = block' id (stmt : prev') : swallow n in
            case l of
              LastBranch tgt ->
                  case n of
                    -- THIS OPT IS WRONG -- LABELS CAN SHOW UP ELSEWHERE IN THE GRAPH
                    --G.Block id' _ t : bs
                    --    | tgt == id', unique_pred id' 
                    --    -> tail id prev' t bs -- optimize out redundant labels
                    _ -> endblock (CmmBranch tgt)
              LastCondBranch expr tid fid ->
                  case n of
                    G.Block id' t : bs
                      -- It would be better to handle earlier, but we still must
                      -- generate correct code here.
                      | id' == fid, tid == fid, unique_pred id' ->
                                 tail id prev' t bs
                      | id' == fid, unique_pred id' ->
                                 tail id (CmmCondBranch expr tid : prev') t bs
                      | id' == tid, unique_pred id',
                        Just e' <- maybeInvertCmmExpr expr ->
                                 tail id (CmmCondBranch e'   fid : prev') t bs
                    _ -> let instrs' = CmmBranch fid : CmmCondBranch expr tid : prev'
                         in block' id instrs' : swallow n
              LastSwitch arg ids   -> endblock $ CmmSwitch arg $ ids
              LastCall e _ _ _ _ -> endblock $ CmmJump e []
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
                    in  case lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeBlockSet s == 1 then
                                        extendBlockSet single id
                                    else single
              in  G.fold_blocks add emptyBlockSet g
          unique_pred id = elemBlockSet id single_preds
          call_succs = 
              let add b succs =
                      case G.last (G.unzip b) of
                        G.LastOther (LastCall _ (Just id) _ _ _) ->
                          extendBlockSet succs id
                        _ -> succs
              in  G.fold_blocks add emptyBlockSet g
          _is_call_succ id = elemBlockSet id call_succs

scomment :: String -> CmmStmt
scomment s = CmmComment $ mkFastString s

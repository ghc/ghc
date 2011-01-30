{-# LANGUAGE GADTs #-}
-- ToDo: remove
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CmmCvt
  ( cmmToZgraph, cmmOfZgraph )
where

import BlockId
import Cmm
import CmmDecl
import CmmExpr
import MkGraph
import qualified OldCmm as Old
import OldPprCmm ()

import Compiler.Hoopl hiding ((<*>), mkLabel, mkBranch)
import Control.Monad
import Data.Maybe
import Maybes
import Outputable
import UniqSupply

cmmToZgraph :: Old.Cmm -> UniqSM Cmm
cmmOfZgraph :: Cmm     -> Old.Cmm

cmmToZgraph (Cmm tops) = liftM Cmm $ mapM mapTop tops
  where mapTop (CmmProc (Old.CmmInfo _ _ info_tbl) l g) =
          do (stack_info, g) <- toZgraph (showSDoc $ ppr l) g
             return $ CmmProc (TopInfo {info_tbl=info_tbl, stack_info=stack_info}) l g
        mapTop (CmmData s ds) = return $ CmmData s ds
cmmOfZgraph (Cmm tops) = Cmm $ map mapTop tops
  where mapTop (CmmProc h l g) = CmmProc (Old.CmmInfo Nothing Nothing (info_tbl h)) l (ofZgraph g)
        mapTop (CmmData s ds) = CmmData s ds

toZgraph :: String -> Old.ListGraph Old.CmmStmt -> UniqSM (CmmStackInfo, CmmGraph)
toZgraph _ (Old.ListGraph []) =
  do g <- lgraphOfAGraph emptyAGraph
     return (StackInfo {arg_space=0, updfr_space=Nothing}, g)
toZgraph fun_name g@(Old.ListGraph (Old.BasicBlock id ss : other_blocks)) = 
           let (offset, entry) = mkCallEntry NativeNodeCall [] in
           do g <- labelAGraph id $
                     entry <*> mkStmts ss <*> foldr addBlock emptyAGraph other_blocks
              return (StackInfo {arg_space = offset, updfr_space = Nothing}, g)
  where addBlock (Old.BasicBlock id ss) g =
          mkLabel id <*> mkStmts ss <*> g
        updfr_sz = 0 -- panic "upd frame size lost in cmm conversion"
        mkStmts (Old.CmmNop        : ss)  = mkNop        <*> mkStmts ss 
        mkStmts (Old.CmmComment s  : ss)  = mkComment s  <*> mkStmts ss
        mkStmts (Old.CmmAssign l r : ss)  = mkAssign l r <*> mkStmts ss
        mkStmts (Old.CmmStore  l r : ss)  = mkStore  l r <*> mkStmts ss
        mkStmts (Old.CmmCall (Old.CmmCallee f conv) res args (Old.CmmSafe _) Old.CmmMayReturn : ss) =
            mkCall f (conv', conv') (map Old.hintlessCmm res) (map Old.hintlessCmm args) updfr_sz
            <*> mkStmts ss
              where conv' = Foreign (ForeignConvention conv [] []) -- JD: DUBIOUS
        mkStmts (Old.CmmCall (Old.CmmPrim {}) _ _ (Old.CmmSafe _) _ : _) =
            panic "safe call to a primitive CmmPrim CallishMachOp"
        mkStmts (Old.CmmCall f res args Old.CmmUnsafe Old.CmmMayReturn : ss) =
                      mkUnsafeCall (convert_target f res args)
                        (strip_hints res) (strip_hints args)
                      <*> mkStmts ss
        mkStmts (Old.CmmCondBranch e l : fbranch) =
            mkCmmIfThenElse e (mkBranch l) (mkStmts fbranch)
        mkStmts (last : []) = mkLast last
        mkStmts []          = bad "fell off end"
        mkStmts (_ : _ : _) = bad "last node not at end"
        bad msg = pprPanic (msg ++ " in function " ++ fun_name) (ppr g)
        mkLast (Old.CmmCall (Old.CmmCallee f conv) []     args _ Old.CmmNeverReturns) =
            mkFinalCall f conv (map Old.hintlessCmm args) updfr_sz
        mkLast (Old.CmmCall (Old.CmmPrim {}) _ _ _ Old.CmmNeverReturns) =
            panic "Call to CmmPrim never returns?!"
        mkLast (Old.CmmSwitch scrutinee table) = mkSwitch scrutinee table
        -- SURELY, THESE HINTLESS ARGS ARE WRONG AND WILL BE FIXED WHEN CALLING
        -- CONVENTIONS ARE HONORED?
        mkLast (Old.CmmJump tgt args)          = mkJump   tgt (map Old.hintlessCmm args) updfr_sz
        mkLast (Old.CmmReturn ress)            =
          mkReturnSimple (map Old.hintlessCmm ress) updfr_sz
        mkLast (Old.CmmBranch tgt)             = mkBranch tgt
        mkLast (Old.CmmCall _f (_:_) _args _ Old.CmmNeverReturns) =
                   panic "Call never returns but has results?!"
        mkLast _ = panic "fell off end of block"

strip_hints :: [Old.CmmHinted a] -> [a]
strip_hints = map Old.hintlessCmm

convert_target :: Old.CmmCallTarget -> Old.HintedCmmFormals -> Old.HintedCmmActuals -> ForeignTarget
convert_target (Old.CmmCallee e cc) ress  args  = ForeignTarget e (ForeignConvention cc (map Old.cmmHint args) (map Old.cmmHint ress))
convert_target (Old.CmmPrim op)	   _ress _args = PrimTarget op

data ValueDirection = Arguments | Results

add_hints :: Convention -> ValueDirection -> [a] -> [Old.CmmHinted a]
add_hints conv vd args = zipWith Old.CmmHinted args (get_hints conv vd)

get_hints :: Convention -> ValueDirection -> [ForeignHint]
get_hints (Foreign (ForeignConvention _ hints _)) Arguments = hints
get_hints (Foreign (ForeignConvention _ _ hints)) Results   = hints
get_hints _other_conv                             _vd       = repeat NoHint

get_conv :: ForeignTarget -> Convention
get_conv (PrimTarget _)       = NativeNodeCall -- JD: SUSPICIOUS
get_conv (ForeignTarget _ fc) = Foreign fc

cmm_target :: ForeignTarget -> Old.CmmCallTarget
cmm_target (PrimTarget op) = Old.CmmPrim op
cmm_target (ForeignTarget e (ForeignConvention cc _ _)) = Old.CmmCallee e cc

ofZgraph :: CmmGraph -> Old.ListGraph Old.CmmStmt
ofZgraph g = Old.ListGraph $ mapMaybe convert_block $ postorderDfs g
  -- We catenated some blocks in the conversion process,
  -- because of the CmmCondBranch -- the machine code does not have
  -- 'jump here or there' instruction, but has 'jump if true' instruction.
  -- As OldCmm has the same instruction, so we use it.
  -- When we are doing this, we also catenate normal goto-s (it is for free).

  -- Exactly, we catenate blocks with nonentry labes, that are
  --   a) mentioned exactly once as a successor
  --   b) any of 1) are a target of a goto
  --             2) are false branch target of a conditional jump
  --             3) are true branch target of a conditional jump, and
  --                  the false branch target is a successor of at least 2 blocks
  --                  and the condition can be inverted
  -- The complicated rule 3) is here because we need to assign at most one
  -- catenable block to a CmmCondBranch.
    where preds :: BlockEnv [CmmNode O C]
          preds = mapFold add mapEmpty $ toBlockMap g
            where add block env = foldr (add' $ lastNode block) env (successors block)
                  add' :: CmmNode O C -> BlockId -> BlockEnv [CmmNode O C] -> BlockEnv [CmmNode O C]
                  add' node succ env = mapInsert succ (node : (mapLookup succ env `orElse` [])) env

          to_be_catenated :: BlockId -> Bool
          to_be_catenated id | id == g_entry g = False
                             | Just [CmmBranch _] <- mapLookup id preds = True
                             | Just [CmmCondBranch _ _ f] <- mapLookup id preds
                             , f == id = True
                             | Just [CmmCondBranch e t f] <- mapLookup id preds
                             , t == id
                             , Just (_:_:_) <- mapLookup f preds
                             , Just _ <- maybeInvertCmmExpr e = True
          to_be_catenated _ = False

          convert_block block | to_be_catenated (entryLabel block) = Nothing
          convert_block block = Just $ foldBlockNodesB3 (first, middle, last) block ()
            where first :: CmmNode C O -> [Old.CmmStmt] -> Old.CmmBasicBlock
                  first (CmmEntry bid) stmts = Old.BasicBlock bid stmts

                  middle :: CmmNode O O -> [Old.CmmStmt] -> [Old.CmmStmt]
                  middle node stmts = stmt : stmts
                    where stmt :: Old.CmmStmt
                          stmt = case node of
                            CmmComment s                                   -> Old.CmmComment s
                            CmmAssign l r                                  -> Old.CmmAssign l r
                            CmmStore  l r                                  -> Old.CmmStore  l r
                            CmmUnsafeForeignCall (PrimTarget MO_Touch) _ _ -> Old.CmmNop
                            CmmUnsafeForeignCall target ress args          -> 
                              Old.CmmCall (cmm_target target)
                                          (add_hints (get_conv target) Results   ress)
                                          (add_hints (get_conv target) Arguments args)
                                          Old.CmmUnsafe Old.CmmMayReturn

                  last :: CmmNode O C -> () -> [Old.CmmStmt]
                  last node _ = stmts
                    where stmts :: [Old.CmmStmt]
                          stmts = case node of
                            CmmBranch tgt | to_be_catenated tgt -> tail_of tgt
                                          | otherwise -> [Old.CmmBranch tgt]
                            CmmCondBranch expr tid fid
                              | to_be_catenated fid -> Old.CmmCondBranch expr tid : tail_of fid
                              | to_be_catenated tid
                              , Just expr' <- maybeInvertCmmExpr expr -> Old.CmmCondBranch expr' fid : tail_of tid
                              | otherwise -> [Old.CmmCondBranch expr tid, Old.CmmBranch fid]
                            CmmSwitch arg ids -> [Old.CmmSwitch arg ids]
                            CmmCall e _ _ _ _ -> [Old.CmmJump e []]
                            CmmForeignCall {} -> panic "ofZgraph: CmmForeignCall"
                          tail_of bid = case foldBlockNodesB3 (first, middle, last) block () of
                                          Old.BasicBlock _ stmts -> stmts
                            where Just block = mapLookup bid $ toBlockMap g

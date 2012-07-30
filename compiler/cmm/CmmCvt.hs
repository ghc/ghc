{-# LANGUAGE GADTs #-}
-- ToDo: remove -fno-warn-incomplete-patterns
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CmmCvt
  ( cmmOfZgraph )
where

import BlockId
import Cmm
import CmmUtils
import qualified OldCmm as Old
import OldPprCmm ()

import Hoopl
import Data.Maybe
import Maybes
import Outputable

cmmOfZgraph :: CmmGroup -> Old.CmmGroup
cmmOfZgraph tops = map mapTop tops
  where mapTop (CmmProc h l g) = CmmProc (info_tbls h) l (ofZgraph g)
        mapTop (CmmData s ds) = CmmData s ds

data ValueDirection = Arguments | Results

add_hints :: ForeignTarget -> ValueDirection -> [a] -> [Old.CmmHinted a]
add_hints conv vd args = zipWith Old.CmmHinted args (get_hints conv vd)

get_hints :: ForeignTarget -> ValueDirection -> [ForeignHint]
get_hints (ForeignTarget _ (ForeignConvention _ hints _)) Arguments = hints
get_hints (ForeignTarget _ (ForeignConvention _ _ hints)) Results   = hints
get_hints (PrimTarget _) _vd = repeat NoHint

cmm_target :: ForeignTarget -> Old.CmmCallTarget
cmm_target (PrimTarget op) = Old.CmmPrim op Nothing
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
                                          (add_hints target Results   ress)
                                          (add_hints target Arguments args)
                                          Old.CmmMayReturn

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
                            -- ToDo: STG Live
                            CmmCall e _ r _ _ _ -> [Old.CmmJump e (Just r)]
                            CmmForeignCall {} -> panic "ofZgraph: CmmForeignCall"
                          tail_of bid = case foldBlockNodesB3 (first, middle, last) block () of
                                          Old.BasicBlock _ stmts -> stmts
                            where Just block = mapLookup bid $ toBlockMap g


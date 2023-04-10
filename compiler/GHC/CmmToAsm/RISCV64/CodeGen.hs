{-# LANGUAGE GADTs #-}
module GHC.CmmToAsm.RISCV64.CodeGen where

import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.RISCV64.Instr
import Prelude
import GHC.Cmm
import GHC.Cmm.Utils
import Control.Monad
import GHC.Cmm.Dataflow.Block
import GHC.Data.OrdList
import GHC.Cmm.Dataflow
import GHC.Driver.Ppr ( showPprUnsafe, showSDocUnsafe )
import GHC.Plugins (Outputable)
import GHC.Utils.Outputable

-- | Don't try to compile all GHC Cmm files in the beginning.
-- Ignore them. There's a flag to decide we really want to emit something.
emptyCmmTopCodeGen
        :: RawCmmDecl
        -> NatM [NatCmmDecl RawCmmStatics Instr]
-- "TODO: cmmTopCodeGen"
emptyCmmTopCodeGen _ = return []

cmmTopCodeGen
        :: RawCmmDecl
        -> NatM [NatCmmDecl RawCmmStatics Instr]
-- "TODO: cmmTopCodeGen"
cmmTopCodeGen (CmmProc info lab live graph) = do
  let blocks = toBlockListEntryFirst graph
  (nat_blocks,statics) <- mapAndUnzipM basicBlockCodeGen blocks
  let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      tops = proc : concat statics
  return tops
cmmTopCodeGen (CmmData sec dat) =
  return [CmmData sec dat]  -- no translation, we just use CmmStatic

basicBlockCodeGen
        :: Block CmmNode C C
        -> NatM ( [NatBasicBlock Instr]
                , [NatCmmDecl RawCmmStatics Instr])
basicBlockCodeGen block = do
  let (_, nodes, tail)  = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
      loc_instrs = nilOL
  mid_instrs <- stmtsToInstrs stmts
  tail_instrs <- stmtToInstrs tail
  let instrs = loc_instrs `appOL` mid_instrs `appOL` tail_instrs
  let
        (top,other_blocks,statics) = foldrOL mkBlocks ([],[],[]) instrs

        mkBlocks (NEWBLOCK id) (instrs,blocks,statics)
          = ([], BasicBlock id instrs : blocks, statics)
        mkBlocks (LDATA sec dat) (instrs,blocks,statics)
          = error "TODO: basicBlockCodeGen" -- (instrs, blocks, CmmData sec dat:statics)
        mkBlocks instr (instrs,blocks,statics)
          = (instr:instrs, blocks, statics)
  return (BasicBlock id top : other_blocks, statics)

type InstrBlock
        = OrdList Instr

stmtsToInstrs :: [CmmNode e x] -> NatM InstrBlock
stmtsToInstrs stmts
   = do instrss <- mapM stmtToInstrs stmts
        return (concatOL instrss)

stmtToInstrs :: CmmNode e x -> NatM InstrBlock
stmtToInstrs stmt = do
  platform <- getPlatform
  case stmt of
    CmmComment s   -> return (unitOL (COMMENT s))
    a -> error $ "TODO: stmtToInstrs" ++ (showSDocUnsafe . pdoc platform) a

generateJumpTableForInstr :: Instr
                          -> Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr _ = error "TODO: generateJumpTableForInstr"

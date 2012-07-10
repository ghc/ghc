-----------------------------------------------------------------------------
--
-- Old-style Cmm data types
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module OldCmm (
        CmmGroup, GenCmmGroup, RawCmmGroup, CmmDecl, RawCmmDecl,
        ListGraph(..),
        CmmInfoTable(..), ClosureTypeInfo(..),
        CmmStatic(..), CmmStatics(..), CmmFormal, CmmActual,

        cmmMapGraph, cmmTopMapGraph,

        GenBasicBlock(..), CmmBasicBlock, blockId, blockStmts, mapBlockStmts,

        CmmStmt(..), CmmReturnInfo(..), CmmHinted(..),
        HintedCmmFormal, HintedCmmActual,

        CmmSafety(..), CmmCallTarget(..),
        New.GenCmmDecl(..), New.ForeignHint(..),

        module CmmExpr,

        Section(..), ProfilingInfo(..), C_SRT(..)
    ) where

#include "HsVersions.h"

import qualified Cmm as New
import Cmm ( CmmInfoTable(..), GenCmmGroup, CmmStatics(..), GenCmmDecl(..),
             CmmFormal, CmmActual, Section(..), CmmStatic(..),
             ProfilingInfo(..), ClosureTypeInfo(..) )

import BlockId
import ClosureInfo
import CmmExpr
import FastString
import ForeignCall


-- A [[BlockId]] is a local label.
-- Local labels must be unique within an entire compilation unit, not
-- just a single top-level item, because local labels map one-to-one
-- with assembly-language labels.

-----------------------------------------------------------------------------
--  Cmm, CmmDecl, CmmBasicBlock
-----------------------------------------------------------------------------

-- A file is a list of top-level chunks.  These may be arbitrarily
-- re-orderd during code generation.

-- | A control-flow graph represented as a list of extended basic blocks.
--
-- Code, may be empty.  The first block is the entry point.  The
-- order is otherwise initially unimportant, but at some point the
-- code gen will fix the order.
--
-- BlockIds must be unique across an entire compilation unit, since
-- they are translated to assembly-language labels, which scope
-- across a whole compilation unit.
newtype ListGraph i = ListGraph [GenBasicBlock i]

-- | Cmm with the info table as a data type
type CmmGroup = GenCmmGroup CmmStatics CmmInfoTable (ListGraph CmmStmt)
type CmmDecl = GenCmmDecl CmmStatics CmmInfoTable (ListGraph CmmStmt)

-- | Cmm with the info tables converted to a list of 'CmmStatic' along with the info
-- table label. If we are building without tables-next-to-code there will be no statics
--
-- INVARIANT: if there is an info table, it has at least one CmmStatic
type RawCmmGroup = GenCmmGroup CmmStatics (Maybe CmmStatics) (ListGraph CmmStmt)
type RawCmmDecl = GenCmmDecl CmmStatics (Maybe CmmStatics) (ListGraph CmmStmt)


-- A basic block containing a single label, at the beginning.
-- The list of basic blocks in a top-level code block may be re-ordered.
-- Fall-through is not allowed: there must be an explicit jump at the
-- end of each basic block, but the code generator might rearrange basic
-- blocks in order to turn some jumps into fallthroughs.

data GenBasicBlock i = BasicBlock BlockId [i]
type CmmBasicBlock   = GenBasicBlock CmmStmt

instance UserOfLocalRegs i => UserOfLocalRegs (GenBasicBlock i) where
    foldRegsUsed f set (BasicBlock _ l) = foldRegsUsed f set l

-- | The branch block id is that of the first block in
-- the branch, which is that branch's entry point
blockId :: GenBasicBlock i -> BlockId
blockId (BasicBlock blk_id _ ) = blk_id

blockStmts :: GenBasicBlock i -> [i]
blockStmts (BasicBlock _ stmts) = stmts

mapBlockStmts :: (i -> i') -> GenBasicBlock i -> GenBasicBlock i'
mapBlockStmts f (BasicBlock id bs) = BasicBlock id (map f bs)

----------------------------------------------------------------
--   graph maps
----------------------------------------------------------------

cmmMapGraph    :: (g -> g') -> GenCmmGroup d h g -> GenCmmGroup d h g'
cmmMapGraph f tops = map (cmmTopMapGraph f) tops

cmmTopMapGraph :: (g -> g') -> GenCmmDecl d h g -> GenCmmDecl d h g'
cmmTopMapGraph f (CmmProc h l g) = CmmProc h l (f g)
cmmTopMapGraph _ (CmmData s ds)  = CmmData s ds

data CmmReturnInfo
  = CmmMayReturn
  | CmmNeverReturns
  deriving ( Eq )

-----------------------------------------------------------------------------
--              CmmStmt
-- A "statement".  Note that all branches are explicit: there are no
-- control transfers to computed addresses, except when transfering
-- control to a new function.
-----------------------------------------------------------------------------

data CmmStmt
  = CmmNop
  | CmmComment FastString

  | CmmAssign CmmReg CmmExpr      -- Assign to register

  | CmmStore CmmExpr CmmExpr      -- Assign to memory location. Size is
                                  -- given by cmmExprType of the rhs.

  | CmmCall                       -- A call (foreign, native or primitive), with
      CmmCallTarget
      [HintedCmmFormal]            -- zero or more results
      [HintedCmmActual]            -- zero or more arguments
      CmmReturnInfo
      -- Some care is necessary when handling the arguments of these, see
      -- [Register parameter passing] and the hack in cmm/CmmOpt.hs

  | CmmBranch BlockId             -- branch to another BB in this fn

  | CmmCondBranch CmmExpr BlockId -- conditional branch

  | CmmSwitch                     -- Table branch
      CmmExpr                       -- The scrutinee is zero-based;
      [Maybe BlockId]               --      zero -> first block
                                    --      one  -> second block etc
                                    -- Undefined outside range, and when
                                    -- there's a Nothing

  | CmmJump                       -- Jump to another C-- function,
      CmmExpr                       -- Target
      (Maybe [GlobalReg])           -- Live registers at call site;
                                    --      Nothing -> no information, assume
                                    --                 all live
                                    --      Just .. -> info on liveness, []
                                    --                 means no live registers
                                    -- This isn't all 'live' registers, just
                                    -- the argument STG registers that are live
                                    -- AND also possibly mapped to machine
                                    -- registers. (So Sp, Hp, HpLim... ect
                                    -- are never included here as they are
                                    -- always live, only R2.., D1.. are
                                    -- on this list)

  | CmmReturn                     -- Return from a native C-- function,

data CmmHinted a
  = CmmHinted {
        hintlessCmm :: a,
        cmmHint :: New.ForeignHint
    }
  deriving( Eq )

type HintedCmmFormal = CmmHinted CmmFormal
type HintedCmmActual = CmmHinted CmmActual

data CmmSafety
  = CmmUnsafe
  | CmmSafe C_SRT
  | CmmInterruptible

-- | enable us to fold used registers over '[CmmActual]' and '[CmmFormal]'
instance UserOfLocalRegs CmmStmt where
  foldRegsUsed f (set::b) s = stmt s set
    where
      stmt :: CmmStmt -> b -> b
      stmt (CmmNop)                  = id
      stmt (CmmComment {})           = id
      stmt (CmmAssign _ e)           = gen e
      stmt (CmmStore e1 e2)          = gen e1 . gen e2
      stmt (CmmCall target _ es _)   = gen target . gen es
      stmt (CmmBranch _)             = id
      stmt (CmmCondBranch e _)       = gen e
      stmt (CmmSwitch e _)           = gen e
      stmt (CmmJump e _)             = gen e
      stmt (CmmReturn)               = id

      gen :: UserOfLocalRegs a => a -> b -> b
      gen a set = foldRegsUsed f set a

instance UserOfLocalRegs CmmCallTarget where
    foldRegsUsed f set (CmmCallee e _)    = foldRegsUsed f set e
    foldRegsUsed f set (CmmPrim _ mStmts) = foldRegsUsed f set mStmts

instance UserOfLocalRegs a => UserOfLocalRegs (CmmHinted a) where
    foldRegsUsed f set a = foldRegsUsed f set (hintlessCmm a)

instance DefinerOfLocalRegs a => DefinerOfLocalRegs (CmmHinted a) where
    foldRegsDefd f set a = foldRegsDefd f set (hintlessCmm a)

{-
Discussion
~~~~~~~~~~

One possible problem with the above type is that the only way to do a
non-local conditional jump is to encode it as a branch to a block that
contains a single jump.  This leads to inefficient code in the back end.

[N.B. This problem will go away when we make the transition to the
'zipper' form of control-flow graph, in which both targets of a
conditional jump are explicit. ---NR]

One possible way to fix this would be:

data CmmStat =
  ...
  | CmmJump CmmBranchDest
  | CmmCondJump CmmExpr CmmBranchDest
  ...

data CmmBranchDest
  = Local BlockId
  | NonLocal CmmExpr [LocalReg]

In favour:

+ one fewer constructors in CmmStmt
+ allows both cond branch and switch to jump to non-local destinations

Against:

- not strictly necessary: can already encode as branch+jump
- not always possible to implement any better in the back end
- could do the optimisation in the back end (but then plat-specific?)
- C-- doesn't have it
- back-end optimisation might be more general (jump shortcutting)

So we'll stick with the way it is, and add the optimisation to the NCG.
-}

-----------------------------------------------------------------------------
--              CmmCallTarget
--
-- The target of a CmmCall.
-----------------------------------------------------------------------------

data CmmCallTarget
  = CmmCallee           -- Call a function (foreign or native)
        CmmExpr                 -- literal label <=> static call
                                -- other expression <=> dynamic call
        CCallConv               -- The calling convention

  | CmmPrim             -- Call a "primitive" (eg. sin, cos)
        CallishMachOp           -- These might be implemented as inline
                                -- code by the backend.
        -- If we don't know how to implement the
        -- mach op, then we can replace it with
        -- this list of statements:
        (Maybe [CmmStmt])


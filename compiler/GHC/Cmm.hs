-- Cmm representations using Hoopl's Graph CmmNode e x.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module GHC.Cmm (
     -- * Cmm top-level datatypes
     CmmProgram, CmmGroup, CmmGroupSRTs, RawCmmGroup, GenCmmGroup,
     CmmDecl, CmmDeclSRTs, GenCmmDecl(..),
     CmmGraph, GenCmmGraph(..),
     CmmBlock, RawCmmDecl,
     Section(..), SectionType(..),
     GenCmmStatics(..), type CmmStatics, type RawCmmStatics, CmmStatic(..),
     isSecConstant,

     -- ** Blocks containing lists
     GenBasicBlock(..), blockId,
     ListGraph(..), pprBBlock,

     -- * Info Tables
     CmmTopInfo(..), CmmStackInfo(..), CmmInfoTable(..), topInfoTable,
     ClosureTypeInfo(..),
     ProfilingInfo(..), ConstrDescription,

     -- * Statements, expressions and types
     module GHC.Cmm.Node,
     module GHC.Cmm.Expr,
  ) where

import GhcPrelude

import GHC.Types.Id
import GHC.Types.CostCentre
import GHC.Cmm.CLabel
import GHC.Cmm.BlockId
import GHC.Cmm.Node
import GHC.Runtime.Heap.Layout
import GHC.Cmm.Expr
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import Outputable
import Data.ByteString (ByteString)

-----------------------------------------------------------------------------
--  Cmm, GenCmm
-----------------------------------------------------------------------------

-- A CmmProgram is a list of CmmGroups
-- A CmmGroup is a list of top-level declarations

-- When object-splitting is on, each group is compiled into a separate
-- .o file. So typically we put closely related stuff in a CmmGroup.
-- Section-splitting follows suit and makes one .text subsection for each
-- CmmGroup.

type CmmProgram = [CmmGroup]

type GenCmmGroup d h g = [GenCmmDecl d h g]
-- | Cmm group before SRT generation
type CmmGroup     = GenCmmGroup CmmStatics    CmmTopInfo               CmmGraph
-- | Cmm group with SRTs
type CmmGroupSRTs = GenCmmGroup RawCmmStatics CmmTopInfo               CmmGraph
-- | "Raw" cmm group (TODO (osa): not sure what that means)
type RawCmmGroup  = GenCmmGroup RawCmmStatics (LabelMap RawCmmStatics) CmmGraph

-----------------------------------------------------------------------------
--  CmmDecl, GenCmmDecl
-----------------------------------------------------------------------------

-- GenCmmDecl is abstracted over
--   d, the type of static data elements in CmmData
--   h, the static info preceding the code of a CmmProc
--   g, the control-flow graph of a CmmProc
--
-- We expect there to be two main instances of this type:
--   (a) C--, i.e. populated with various C-- constructs
--   (b) Native code, populated with data/instructions

-- | A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmDecl d h g
  = CmmProc     -- A procedure
     h                 -- Extra header such as the info table
     CLabel            -- Entry label
     [GlobalReg]       -- Registers live on entry. Note that the set of live
                       -- registers will be correct in generated C-- code, but
                       -- not in hand-written C-- code. However,
                       -- splitAtProcPoints calculates correct liveness
                       -- information for CmmProcs.
     g                 -- Control-flow graph for the procedure's code

  | CmmData     -- Static data
        Section
        d

type CmmDecl     = GenCmmDecl CmmStatics    CmmTopInfo CmmGraph
type CmmDeclSRTs = GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph

type RawCmmDecl
   = GenCmmDecl
        RawCmmStatics
        (LabelMap RawCmmStatics)
        CmmGraph

-----------------------------------------------------------------------------
--     Graphs
-----------------------------------------------------------------------------

type CmmGraph = GenCmmGraph CmmNode
data GenCmmGraph n = CmmGraph { g_entry :: BlockId, g_graph :: Graph n C C }
type CmmBlock = Block CmmNode C C

-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

-- | CmmTopInfo is attached to each CmmDecl (see defn of CmmGroup), and contains
-- the extra info (beyond the executable code) that belongs to that CmmDecl.
data CmmTopInfo   = TopInfo { info_tbls  :: LabelMap CmmInfoTable
                            , stack_info :: CmmStackInfo }

topInfoTable :: GenCmmDecl a CmmTopInfo (GenCmmGraph n) -> Maybe CmmInfoTable
topInfoTable (CmmProc infos _ _ g) = mapLookup (g_entry g) (info_tbls infos)
topInfoTable _                     = Nothing

data CmmStackInfo
   = StackInfo {
       arg_space :: ByteOff,
               -- number of bytes of arguments on the stack on entry to the
               -- the proc.  This is filled in by GHC.StgToCmm.codeGen, and
               -- used by the stack allocator later.
       updfr_space :: Maybe ByteOff,
               -- XXX: this never contains anything useful, but it should.
               -- See comment in GHC.Cmm.LayoutStack.
       do_layout :: Bool
               -- Do automatic stack layout for this proc.  This is
               -- True for all code generated by the code generator,
               -- but is occasionally False for hand-written Cmm where
               -- we want to do the stack manipulation manually.
  }

-- | Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable {
      cit_lbl  :: CLabel, -- Info table label
      cit_rep  :: SMRep,
      cit_prof :: ProfilingInfo,
      cit_srt  :: Maybe CLabel,   -- empty, or a closure address
      cit_clo  :: Maybe (Id, CostCentreStack)
        -- Just (id,ccs) <=> build a static closure later
        -- Nothing <=> don't build a static closure
        --
        -- Static closures for FUNs and THUNKs are *not* generated by
        -- the code generator, because we might want to add SRT
        -- entries to them later (for FUNs at least; THUNKs are
        -- treated the same for consistency). See Note [SRTs] in
        -- GHC.Cmm.Info.Build, in particular the [FUN] optimisation.
        --
        -- This is strictly speaking not a part of the info table that
        -- will be finally generated, but it's the only convenient
        -- place to convey this information from the code generator to
        -- where we build the static closures in
        -- GHC.Cmm.Info.Build.doSRTs.
    }

data ProfilingInfo
  = NoProfilingInfo
  | ProfilingInfo ByteString ByteString -- closure_type, closure_desc

-----------------------------------------------------------------------------
--              Static Data
-----------------------------------------------------------------------------

data SectionType
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
  | ReadOnlyData16      -- .rodata.cst16 on x86_64, 16-byte aligned
  | CString
  | OtherSection String
  deriving (Show)

-- | Should a data in this section be considered constant
isSecConstant :: Section -> Bool
isSecConstant (Section t _) = case t of
    Text                    -> True
    ReadOnlyData            -> True
    RelocatableReadOnlyData -> True
    ReadOnlyData16          -> True
    CString                 -> True
    Data                    -> False
    UninitialisedData       -> False
    (OtherSection _)        -> False

data Section = Section SectionType CLabel

data CmmStatic
  = CmmStaticLit CmmLit
        -- ^ a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
        -- ^ uninitialised data, N bytes long
  | CmmString ByteString
        -- ^ string of 8-bit values only, not zero terminated.
  | CmmFileEmbed FilePath
        -- ^ an embedded binary file

-- Static data before SRT generation
data GenCmmStatics (rawOnly :: Bool) where
    CmmStatics
      :: CLabel       -- Label of statics
      -> CmmInfoTable
      -> CostCentreStack
      -> [CmmLit]     -- Payload
      -> GenCmmStatics 'False

    -- | Static data, after SRTs are generated
    CmmStaticsRaw
      :: CLabel       -- Label of statics
      -> [CmmStatic]  -- The static data itself
      -> GenCmmStatics a

type CmmStatics    = GenCmmStatics 'False
type RawCmmStatics = GenCmmStatics 'True

-- -----------------------------------------------------------------------------
-- Basic blocks consisting of lists

-- These are used by the LLVM and NCG backends, when populating Cmm
-- with lists of instructions.

data GenBasicBlock i = BasicBlock BlockId [i]

-- | The branch block id is that of the first block in
-- the branch, which is that branch's entry point
blockId :: GenBasicBlock i -> BlockId
blockId (BasicBlock blk_id _ ) = blk_id

newtype ListGraph i = ListGraph [GenBasicBlock i]

instance Outputable instr => Outputable (ListGraph instr) where
    ppr (ListGraph blocks) = vcat (map ppr blocks)

instance Outputable instr => Outputable (GenBasicBlock instr) where
    ppr = pprBBlock

pprBBlock :: Outputable stmt => GenBasicBlock stmt -> SDoc
pprBBlock (BasicBlock ident stmts) =
    hang (ppr ident <> colon) 4 (vcat (map ppr stmts))


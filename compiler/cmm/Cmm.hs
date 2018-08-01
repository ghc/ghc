-- Cmm representations using Hoopl's Graph CmmNode e x.
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module Cmm (
     -- * Cmm top-level datatypes
     CmmProgram, CmmGroup, GenCmmGroup,
     CmmDecl, GenCmmDecl(..),
     CmmGraph, GenCmmGraph(..),
     CmmBlock,
     RawCmmDecl, RawCmmGroup,
     Section(..), SectionType(..), CmmStatics(..), CmmStatic(..),
     isSecConstant,

     -- ** Blocks containing lists
     GenBasicBlock(..), blockId,
     ListGraph(..), pprBBlock,

     -- * Info Tables
     CmmTopInfo(..), CmmStackInfo(..), CmmInfoTable(..), topInfoTable,
     ClosureTypeInfo(..),
     ProfilingInfo(..), ConstrDescription,

     -- * Statements, expressions and types
     module CmmNode,
     module CmmExpr,
  ) where

import GhcPrelude

import Id
import CostCentre
import CLabel
import BlockId
import CmmNode
import SMRep
import CmmExpr
import Hoopl.Block
import Hoopl.Collections
import Hoopl.Graph
import Hoopl.Label
import Outputable

import Data.Word        ( Word8 )

import Binary

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
type CmmGroup = GenCmmGroup CmmStatics CmmTopInfo CmmGraph
type RawCmmGroup = GenCmmGroup CmmStatics (LabelMap CmmStatics) CmmGraph

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

type CmmDecl = GenCmmDecl CmmStatics CmmTopInfo CmmGraph

type RawCmmDecl
   = GenCmmDecl
        CmmStatics
        (LabelMap CmmStatics)
        CmmGraph

instance (Binary d, Binary h, Binary g) => Binary (GenCmmDecl d h g) where
  put_ bh (CmmProc a b c d) = putByte bh 0 >> put_ bh a >> put_ bh b >> put_ bh c >> put_ bh d
  put_ bh (CmmData a b)     = putByte bh 1 >> put_ bh a >> put_ bh b
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> CmmProc <$> get bh <*> get bh <*> get bh <*> get bh
      1 -> CmmData <$> get bh <*> get bh
      _ -> fail "Binary.putGenCmmDecl: invalid tag"

-----------------------------------------------------------------------------
--     Graphs
-----------------------------------------------------------------------------

type CmmGraph = GenCmmGraph CmmNode
data GenCmmGraph n = CmmGraph { g_entry :: BlockId, g_graph :: Graph n C C }
type CmmBlock = Block CmmNode C C

-- These binary instances are pretty stupid;
-- we might have a better solution if we adopt something
-- like https://www.well-typed.com/blog/2017/06/rtti/

instance Binary CmmGraph where
  put_ bh (CmmGraph a b) = put_ bh a >> put_ bh b
  get bh = CmmGraph <$> get bh <*> get bh

-- Note: C C can only be GMany
instance Binary (Graph CmmNode C C) where
  put_ bh (GMany entry body exit)
    = put_ bh entry >> put_ bh body >> put_ bh exit
  get bh = GMany <$> get bh <*> get bh <*> get bh

instance Binary (MaybeO C (Block CmmNode O C)) where
  put_ bh (NothingO) = putByte bh 0
  put_ bh (JustO x) = putByte bh 1 >> put_ bh x

  get bh = do
    tag <- getByte bh
    case tag of
      0 -> pure NothingO
--      1 -> JustO <$> get bh
      _ -> fail "Binary.putMaybeO: invalidTag"

instance Binary (MaybeO C (Block CmmNode C O)) where
  put_ bh (NothingO) = putByte bh 0
  put_ bh (JustO x) = putByte bh 1 >> put_ bh x

  get bh = do
    tag <- getByte bh
    case tag of
      0 -> pure NothingO
--      1 -> JustO <$> get bh
      _ -> fail "Binary.putMaybeO: invalidTag"

instance Binary (Block CmmNode O C) where
  put_ bh (BlockOC a b) = put_ bh a >> put_ bh b
  get bh = BlockOC <$> get bh <*> get bh

instance Binary (Block CmmNode C C) where
  put_ bh (BlockCC a b c) = put_ bh a >> put_ bh b >> put_ bh c
  get bh = BlockCC <$> get bh <*> get bh <*> get bh

instance Binary (Block CmmNode C O) where
  put_ bh (BlockCO a b) = put_ bh a >> put_ bh b
  get bh = BlockCO <$> get bh <*> get bh

instance Binary (Block CmmNode O O) where
  put_ bh BNil = putByte bh 0
  put_ bh (BMiddle a) = putByte bh 1 >> put_ bh a
  put_ bh (BCat a b)  = putByte bh 2 >> put_ bh a >> put_ bh b
  put_ bh (BSnoc a b) = putByte bh 3 >> put_ bh a >> put_ bh b
  put_ bh (BCons a b) = putByte bh 4 >> put_ bh a >> put_ bh b
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> pure BNil
      1 -> BMiddle <$> get bh
      2 -> BCat    <$> get bh <*> get bh
      3 -> BSnoc   <$> get bh <*> get bh
      4 -> BCons   <$> get bh <*> get bh
      _ -> fail "Binary.putBlockCmmNodeOO: invalidTag"

-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

data CmmTopInfo   = TopInfo { info_tbls  :: LabelMap CmmInfoTable
                            , stack_info :: CmmStackInfo }

topInfoTable :: GenCmmDecl a CmmTopInfo (GenCmmGraph n) -> Maybe CmmInfoTable
topInfoTable (CmmProc infos _ _ g) = mapLookup (g_entry g) (info_tbls infos)
topInfoTable _                     = Nothing

data CmmStackInfo
   = StackInfo {
       arg_space :: ByteOff,
               -- number of bytes of arguments on the stack on entry to the
               -- the proc.  This is filled in by StgCmm.codeGen, and used
               -- by the stack allocator later.
       updfr_space :: Maybe ByteOff,
               -- XXX: this never contains anything useful, but it should.
               -- See comment in CmmLayoutStack.
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
        -- CmmBuildInfoTables, in particular the [FUN] optimisation.
        --
        -- This is strictly speaking not a part of the info table that
        -- will be finally generated, but it's the only convenient
        -- place to convey this information from the code generator to
        -- where we build the static closures in
        -- CmmBuildInfoTables.doSRTs.
    }

data ProfilingInfo
  = NoProfilingInfo
  | ProfilingInfo [Word8] [Word8] -- closure_type, closure_desc

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

instance Binary SectionType where
  put_ bh Text = putByte bh 0
  put_ bh Data = putByte bh 1
  put_ bh ReadOnlyData = putByte bh 2
  put_ bh RelocatableReadOnlyData = putByte bh 3
  put_ bh UninitialisedData = putByte bh 4
  put_ bh ReadOnlyData16 = putByte bh 5
  put_ bh CString = putByte bh 6
  put_ bh (OtherSection a) = putByte bh 7 >> put_ bh a
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> pure Text
      1 -> pure Data
      2 -> pure ReadOnlyData
      3 -> pure RelocatableReadOnlyData
      4 -> pure UninitialisedData
      5 -> pure ReadOnlyData16
      6 -> pure CString
      7 -> OtherSection <$> get bh
      _ -> fail "Binary.putSectionType: invalid tag"
      

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

instance Binary Section where
  put_ bh (Section a b) = put_ bh a >> put_ bh b
  get bh = Section <$> get bh <*> get bh

data CmmStatic
  = CmmStaticLit CmmLit
        -- a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
        -- uninitialised data, N bytes long
  | CmmString [Word8]
        -- string of 8-bit values only, not zero terminated.

instance Binary CmmStatic where
  put_ bh (CmmStaticLit a) = putByte bh 0 >> put_ bh a
  put_ bh (CmmUninitialised a) = putByte bh 1 >> put_ bh a
  put_ bh (CmmString a) = putByte bh 2 >> put_ bh a
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> CmmStaticLit <$> get bh
      1 -> CmmUninitialised <$> get bh
      2 -> CmmString <$> get bh
      _ -> fail "Binary.putCmmStatic: invalid tag"

data CmmStatics
   = Statics
       CLabel      -- Label of statics
       [CmmStatic] -- The static data itself

instance Binary CmmStatics where
  put_ bh (Statics a b) = put_ bh a >> put_ bh b
  get bh = Statics <$> get bh <*> get bh

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


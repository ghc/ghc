-- Cmm representations using Hoopl's Graph CmmNode e x.
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Cmm (
     -- * Cmm top-level datatypes
     CmmProgram, CmmGroup, GenCmmGroup,
     CmmDecl, GenCmmDecl(..),
     CmmGraph, GenCmmGraph(..),
     CmmBlock,
     Section(..), CmmStatics(..), CmmStatic(..),

     -- * Cmm graphs
     CmmReplGraph, GenCmmReplGraph, CmmFwdRewrite, CmmBwdRewrite,
   
     -- * Info Tables
     CmmTopInfo(..), CmmStackInfo(..), CmmInfoTable(..), topInfoTable,
     ClosureTypeInfo(..), 
     C_SRT(..), needsSRT,
     ProfilingInfo(..), ConstrDescription, 

     -- * Statements, expressions and types
     module CmmNode,
     module CmmExpr,
  ) where

import CLabel
import BlockId
import CmmNode
import SMRep
import CmmExpr
import UniqSupply
import Compiler.Hoopl

import Data.Word        ( Word8 )

#include "HsVersions.h"

-----------------------------------------------------------------------------
--  Cmm, GenCmm
-----------------------------------------------------------------------------

-- A CmmProgram is a list of CmmGroups  
-- A CmmGroup is a list of top-level declarations  

-- When object-splitting is on,each group is compiled into a separate
-- .o file. So typically we put closely related stuff in a CmmGroup.

type CmmProgram = [CmmGroup]

type GenCmmGroup d h g = [GenCmmDecl d h g]
type CmmGroup = GenCmmGroup CmmStatics CmmTopInfo CmmGraph

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
--       (Cmm and RawCmm in OldCmm.hs)
--   (b) Native code, populated with data/instructions

-- | A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmDecl d h g
  = CmmProc     -- A procedure
     h                 -- Extra header such as the info table
     CLabel            -- Entry label
     g                 -- Control-flow graph for the procedure's code

  | CmmData     -- Static data
        Section
        d

type CmmDecl = GenCmmDecl CmmStatics CmmTopInfo CmmGraph

-----------------------------------------------------------------------------
--     Graphs
-----------------------------------------------------------------------------

type CmmGraph = GenCmmGraph CmmNode
data GenCmmGraph n = CmmGraph { g_entry :: BlockId, g_graph :: Graph n C C }
type CmmBlock = Block CmmNode C C

type CmmReplGraph e x = GenCmmReplGraph CmmNode e x
type GenCmmReplGraph n e x = UniqSM (Maybe (Graph n e x))
type CmmFwdRewrite f = FwdRewrite UniqSM CmmNode f
type CmmBwdRewrite f = BwdRewrite UniqSM CmmNode f

-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

data CmmTopInfo   = TopInfo { info_tbls  :: BlockEnv CmmInfoTable
                            , stack_info :: CmmStackInfo }

topInfoTable :: GenCmmDecl a CmmTopInfo (GenCmmGraph n) -> Maybe CmmInfoTable
topInfoTable (CmmProc infos _ g) = mapLookup (g_entry g) (info_tbls infos)
topInfoTable _                   = Nothing

data CmmStackInfo
   = StackInfo {
       arg_space :: ByteOff,
               -- number of bytes of arguments on the stack on entry to the
               -- the proc.  This is filled in by StgCmm.codeGen, and used
               -- by the stack allocator later.
       updfr_space :: Maybe ByteOff
               -- XXX: this never contains anything useful, but it should.
               -- See comment in CmmLayoutStack.
  }

-- | Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable {
      cit_lbl  :: CLabel, -- Info table label
      cit_rep  :: SMRep,
      cit_prof :: ProfilingInfo,
      cit_srt  :: C_SRT
    }

data ProfilingInfo
  = NoProfilingInfo
  | ProfilingInfo [Word8] [Word8] -- closure_type, closure_desc

-- C_SRT is what StgSyn.SRT gets translated to... 
-- we add a label for the table, and expect only the 'offset/length' form

data C_SRT = NoC_SRT
           | C_SRT !CLabel !WordOff !StgHalfWord {-bitmap or escape-}
           deriving (Eq)

needsSRT :: C_SRT -> Bool
needsSRT NoC_SRT       = False
needsSRT (C_SRT _ _ _) = True

-----------------------------------------------------------------------------
--              Static Data
-----------------------------------------------------------------------------

data Section
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
  | ReadOnlyData16      -- .rodata.cst16 on x86_64, 16-byte aligned
  | OtherSection String

data CmmStatic
  = CmmStaticLit CmmLit
        -- a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
        -- uninitialised data, N bytes long
  | CmmString [Word8]
        -- string of 8-bit values only, not zero terminated.

data CmmStatics
   = Statics
       CLabel      -- Label of statics
       [CmmStatic] -- The static data itself


-----------------------------------------------------------------------------
--
-- Cmm data types
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CmmDecl (
        GenCmm(..), GenCmmTop(..),
        CmmInfoTable(..), HasStaticClosure, ClosureTypeInfo(..), ConstrDescription,
        ProfilingInfo(..), ClosureTypeTag,
        CmmActual, CmmFormal, ForeignHint(..),
        CmmStatics(..), CmmStatic(..), Section(..),
  ) where

#include "HsVersions.h"

import CmmExpr
import CLabel
import SMRep
import ClosureInfo

import Data.Word


-- A [[BlockId]] is a local label.
-- Local labels must be unique within an entire compilation unit, not
-- just a single top-level item, because local labels map one-to-one
-- with assembly-language labels.

-----------------------------------------------------------------------------
--  GenCmm, GenCmmTop
-----------------------------------------------------------------------------

-- A file is a list of top-level chunks.  These may be arbitrarily
-- re-orderd during code generation.

-- GenCmm is abstracted over
--   d, the type of static data elements in CmmData
--   h, the static info preceding the code of a CmmProc
--   g, the control-flow graph of a CmmProc
--
-- We expect there to be two main instances of this type:
--   (a) C--, i.e. populated with various C-- constructs
--       (Cmm and RawCmm in OldCmm.hs)
--   (b) Native code, populated with data/instructions
--
-- A second family of instances based on Hoopl is in Cmm.hs.
--
newtype GenCmm d h g = Cmm [GenCmmTop d h g]

-- | A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmTop d h g
  = CmmProc     -- A procedure
     h                 -- Extra header such as the info table
     CLabel            -- Entry label
     g                 -- Control-flow graph for the procedure's code

  | CmmData     -- Static data
        Section
        d


-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

-- Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable
      CLabel -- Info table label
      HasStaticClosure
      ProfilingInfo
      ClosureTypeTag -- Int
      ClosureTypeInfo
  | CmmNonInfoTable   -- Procedure doesn't need an info table

type HasStaticClosure = Bool

-- TODO: The GC target shouldn't really be part of CmmInfo
-- as it doesn't appear in the resulting info table.
-- It should be factored out.

data ClosureTypeInfo
  = ConstrInfo ClosureLayout ConstrTag ConstrDescription
  | FunInfo    ClosureLayout C_SRT FunArity ArgDescr SlowEntry
  | ThunkInfo  ClosureLayout C_SRT
  | ThunkSelectorInfo SelectorOffset C_SRT
  | ContInfo
      [Maybe LocalReg]  -- Stack layout: Just x, an item x
                        --               Nothing: a 1-word gap
                        -- Start of list is the *young* end
      C_SRT

-- TODO: These types may need refinement
data ProfilingInfo = ProfilingInfo CmmLit CmmLit -- closure_type, closure_desc
type ClosureTypeTag = StgHalfWord
type ClosureLayout = (StgHalfWord, StgHalfWord) -- ptrs, nptrs
type ConstrTag = StgHalfWord
type ConstrDescription = CmmLit
type FunArity = StgHalfWord
type SlowEntry = CmmLit
  -- We would like this to be a CLabel but
  -- for now the parser sets this to zero on an INFO_TABLE_FUN.
type SelectorOffset = StgWord

type CmmActual = CmmExpr
type CmmFormal = LocalReg

data ForeignHint
  = NoHint | AddrHint | SignedHint
  deriving( Eq )
        -- Used to give extra per-argument or per-result
        -- information needed by foreign calling conventions

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

data CmmStatics = Statics CLabel {- Label of statics -} [CmmStatic] {- The static data itself -}

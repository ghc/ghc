{-# LANGUAGE MagicHash, RecordWildCards, GeneralizedNewtypeDeriving #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler types
module ByteCodeTypes
  ( CompiledByteCode(..), seqCompiledByteCode, FFIInfo(..)
  , UnlinkedBCO(..), BCOPtr(..), BCONPtr(..)
  , ItblEnv, ItblPtr(..)
  , CgBreakInfo(..)
  , ModBreaks (..), BreakIndex, emptyModBreaks
  , CCostCentre
  ) where

import GhcPrelude

import FastString
import Id
import Name
import NameEnv
import Outputable
import PrimOp
import SizedSeq
import Type
import SrcLoc
import GHCi.BreakArray
import GHCi.RemoteTypes
import GHCi.FFI
import GHCi.InfoTable
import Control.DeepSeq

import Foreign
import Data.Array
import Data.Array.Base  ( UArray(..) )
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GHC.Stack.CCS

-- -----------------------------------------------------------------------------
-- Compiled Byte Code

data CompiledByteCode = CompiledByteCode
  { bc_bcos   :: [UnlinkedBCO]  -- Bunch of interpretable bindings
  , bc_itbls  :: ItblEnv        -- A mapping from DataCons to their itbls
  , bc_ffis   :: [FFIInfo]      -- ffi blocks we allocated
  , bc_strs   :: [RemotePtr ()] -- malloc'd strings
  , bc_breaks :: Maybe ModBreaks -- breakpoint info (Nothing if we're not
                                 -- creating breakpoints, for some reason)
  }
                -- ToDo: we're not tracking strings that we malloc'd
newtype FFIInfo = FFIInfo (RemotePtr C_ffi_cif)
  deriving (Show, NFData)

instance Outputable CompiledByteCode where
  ppr CompiledByteCode{..} = ppr bc_bcos

-- Not a real NFData instance, because ModBreaks contains some things
-- we can't rnf
seqCompiledByteCode :: CompiledByteCode -> ()
seqCompiledByteCode CompiledByteCode{..} =
  rnf bc_bcos `seq`
  rnf (nameEnvElts bc_itbls) `seq`
  rnf bc_ffis `seq`
  rnf bc_strs `seq`
  rnf (fmap seqModBreaks bc_breaks)

type ItblEnv = NameEnv (Name, ItblPtr)
        -- We need the Name in the range so we know which
        -- elements to filter out when unloading a module

newtype ItblPtr = ItblPtr (RemotePtr StgInfoTable)
  deriving (Show, NFData)

data UnlinkedBCO
   = UnlinkedBCO {
        unlinkedBCOName   :: !Name,
        unlinkedBCOArity  :: {-# UNPACK #-} !Int,
        unlinkedBCOInstrs :: !(UArray Int Word16),      -- insns
        unlinkedBCOBitmap :: !(UArray Int Word64),      -- bitmap
        unlinkedBCOLits   :: !(SizedSeq BCONPtr),       -- non-ptrs
        unlinkedBCOPtrs   :: !(SizedSeq BCOPtr)         -- ptrs
   }

instance NFData UnlinkedBCO where
  rnf UnlinkedBCO{..} =
    rnf unlinkedBCOLits `seq`
    rnf unlinkedBCOPtrs

data BCOPtr
  = BCOPtrName   !Name
  | BCOPtrPrimOp !PrimOp
  | BCOPtrBCO    !UnlinkedBCO
  | BCOPtrBreakArray  -- a pointer to this module's BreakArray

instance NFData BCOPtr where
  rnf (BCOPtrBCO bco) = rnf bco
  rnf x = x `seq` ()

data BCONPtr
  = BCONPtrWord  {-# UNPACK #-} !Word
  | BCONPtrLbl   !FastString
  | BCONPtrItbl  !Name
  | BCONPtrStr   !ByteString

instance NFData BCONPtr where
  rnf x = x `seq` ()

-- | Information about a breakpoint that we know at code-generation time
data CgBreakInfo
   = CgBreakInfo
   { cgb_vars   :: [(Id,Word16)]
   , cgb_resty  :: Type
   }

-- Not a real NFData instance because we can't rnf Id or Type
seqCgBreakInfo :: CgBreakInfo -> ()
seqCgBreakInfo CgBreakInfo{..} =
  rnf (map snd cgb_vars) `seq`
  seqType cgb_resty

instance Outputable UnlinkedBCO where
   ppr (UnlinkedBCO nm _arity _insns _bitmap lits ptrs)
      = sep [text "BCO", ppr nm, text "with",
             ppr (sizeSS lits), text "lits",
             ppr (sizeSS ptrs), text "ptrs" ]

instance Outputable CgBreakInfo where
   ppr info = text "CgBreakInfo" <+>
              parens (ppr (cgb_vars info) <+>
                      ppr (cgb_resty info))

-- -----------------------------------------------------------------------------
-- Breakpoints

-- | Breakpoint index
type BreakIndex = Int

-- | C CostCentre type
data CCostCentre

-- | All the information about the breakpoints for a module
data ModBreaks
   = ModBreaks
   { modBreaks_flags :: ForeignRef BreakArray
        -- ^ The array of flags, one per breakpoint,
        -- indicating which breakpoints are enabled.
   , modBreaks_locs :: !(Array BreakIndex SrcSpan)
        -- ^ An array giving the source span of each breakpoint.
   , modBreaks_vars :: !(Array BreakIndex [OccName])
        -- ^ An array giving the names of the free variables at each breakpoint.
   , modBreaks_decls :: !(Array BreakIndex [String])
        -- ^ An array giving the names of the declarations enclosing each breakpoint.
   , modBreaks_ccs :: !(Array BreakIndex (RemotePtr CostCentre))
        -- ^ Array pointing to cost centre for each breakpoint
   , modBreaks_breakInfo :: IntMap CgBreakInfo
        -- ^ info about each breakpoint from the bytecode generator
   }

seqModBreaks :: ModBreaks -> ()
seqModBreaks ModBreaks{..} =
  rnf modBreaks_flags `seq`
  rnf modBreaks_locs `seq`
  rnf modBreaks_vars `seq`
  rnf modBreaks_decls `seq`
  rnf modBreaks_ccs `seq`
  rnf (fmap seqCgBreakInfo modBreaks_breakInfo)

-- | Construct an empty ModBreaks
emptyModBreaks :: ModBreaks
emptyModBreaks = ModBreaks
   { modBreaks_flags = error "ModBreaks.modBreaks_array not initialised"
         -- ToDo: can we avoid this?
   , modBreaks_locs  = array (0,-1) []
   , modBreaks_vars  = array (0,-1) []
   , modBreaks_decls = array (0,-1) []
   , modBreaks_ccs = array (0,-1) []
   , modBreaks_breakInfo = IntMap.empty
   }

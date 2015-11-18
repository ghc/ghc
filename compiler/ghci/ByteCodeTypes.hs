{-# LANGUAGE MagicHash #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler types
module ByteCodeTypes
  ( CompiledByteCode(..), FFIInfo(..)
  , UnlinkedBCO(..), BCOPtr(..), BCONPtr(..)
  , ItblEnv, ItblPtr(..)
  , BreakInfo(..)
  ) where

import FastString
import Id
import Module
import Name
import NameEnv
import Outputable
import PrimOp
import SizedSeq
import Type

import Foreign
import Data.Array.Base  ( UArray(..) )
import Data.ByteString (ByteString)
import GHC.Exts


data CompiledByteCode
  = ByteCode [UnlinkedBCO] -- Bunch of interpretable bindings
             ItblEnv       -- A mapping from DataCons to their itbls
             [FFIInfo]     -- ffi blocks we allocated
                -- ToDo: we're not tracking strings that we malloc'd

newtype FFIInfo = FFIInfo (Ptr ())
  deriving Show

instance Outputable CompiledByteCode where
  ppr (ByteCode bcos _ _) = ppr bcos

type ItblEnv = NameEnv (Name, ItblPtr)
        -- We need the Name in the range so we know which
        -- elements to filter out when unloading a module

newtype ItblPtr = ItblPtr (Ptr ()) deriving Show

data UnlinkedBCO
   = UnlinkedBCO {
        unlinkedBCOName   :: Name,
        unlinkedBCOArity  :: Int,
        unlinkedBCOInstrs :: UArray Int Word16,         -- insns
        unlinkedBCOBitmap :: UArray Int Word,           -- bitmap
        unlinkedBCOLits   :: (SizedSeq BCONPtr),        -- non-ptrs
        unlinkedBCOPtrs   :: (SizedSeq BCOPtr)          -- ptrs
   }

data BCOPtr
  = BCOPtrName   Name
  | BCOPtrPrimOp PrimOp
  | BCOPtrBCO    UnlinkedBCO
  | BCOPtrBreakInfo  BreakInfo
  | BCOPtrArray (MutableByteArray# RealWorld)

data BCONPtr
  = BCONPtrWord  Word
  | BCONPtrLbl   FastString
  | BCONPtrItbl  Name
  | BCONPtrStr   ByteString

data BreakInfo
   = BreakInfo
   { breakInfo_module :: Module
   , breakInfo_number :: {-# UNPACK #-} !Int
   , breakInfo_vars   :: [(Id,Word16)]
   , breakInfo_resty  :: Type
   }

instance Outputable UnlinkedBCO where
   ppr (UnlinkedBCO nm _arity _insns _bitmap lits ptrs)
      = sep [text "BCO", ppr nm, text "with",
             ppr (sizeSS lits), text "lits",
             ppr (sizeSS ptrs), text "ptrs" ]

instance Outputable BreakInfo where
   ppr info = text "BreakInfo" <+>
              parens (ppr (breakInfo_module info) <+>
                      ppr (breakInfo_number info) <+>
                      ppr (breakInfo_vars info) <+>
                      ppr (breakInfo_resty info))

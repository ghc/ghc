{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode instruction definitions
module GHC.ByteCode.Instr (
        BCInstr(..), ProtoBCO(..), bciStackUse, LocalLabel(..)
  ) where

import GHC.Prelude

import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHCi.FFI (C_ffi_cif)
import GHC.StgToCmm.Layout     ( ArgRep(..) )
import GHC.Utils.Outputable
import GHC.Types.Name
import GHC.Types.Literal
import GHC.Core.DataCon
import GHC.Builtin.PrimOps
import GHC.Runtime.Heap.Layout ( StgWord )

import Data.Int
import Data.Word

#if MIN_VERSION_rts(1,0,3)
import Data.ByteString (ByteString)
#endif

import GHC.Stack.CCS (CostCentre)

import GHC.Stg.Syntax
import GHCi.BreakArray (BreakArray)
import Language.Haskell.Syntax.Module.Name (ModuleName)
import GHC.Types.Unique
import GHC.Unit.Types (UnitId)

-- ----------------------------------------------------------------------------
-- Bytecode instructions

data ProtoBCO a
   = ProtoBCO {
        protoBCOName       :: a,          -- name, in some sense
        protoBCOInstrs     :: [BCInstr],  -- instrs
        -- arity and GC info
        protoBCOBitmap     :: [StgWord],
        protoBCOBitmapSize :: Word,
        protoBCOArity      :: Int,
        -- what the BCO came from, for debugging only
        protoBCOExpr       :: Either [CgStgAlt] CgStgRhs,
        -- malloc'd pointers
        protoBCOFFIs       :: [FFIInfo]
   }

-- | A local block label (e.g. identifying a case alternative).
newtype LocalLabel = LocalLabel { getLocalLabel :: Word32 }
  deriving (Eq, Ord)

-- Just so we can easily juse UniqFM.
instance Uniquable LocalLabel where
  getUnique (LocalLabel w) = mkUniqueGrimily $ fromIntegral w

instance Outputable LocalLabel where
  ppr (LocalLabel lbl) = text "lbl:" <> ppr lbl

data BCInstr
   -- Messing with the stack
   = STKCHECK  !Word

   -- Push locals (existing bits of the stack)
   | PUSH_L    !WordOff{-offset-}
   | PUSH_LL   !WordOff !WordOff{-2 offsets-}
   | PUSH_LLL  !WordOff !WordOff !WordOff{-3 offsets-}

   -- Push the specified local as a 8, 16, 32 bit value onto the stack. (i.e.,
   -- the stack will grow by 8, 16 or 32 bits)
   | PUSH8  !ByteOff
   | PUSH16 !ByteOff
   | PUSH32 !ByteOff

   -- Push the specified local as a 8, 16, 32 bit value onto the stack, but the
   -- value will take the whole word on the stack (i.e., the stack will grow by
   -- a word)
   -- This is useful when extracting a packed constructor field for further use.
   -- Currently we expect all values on the stack to take full words, except for
   -- the ones used for PACK (i.e., actually constructing new data types, in
   -- which case we use PUSH{8,16,32})
   | PUSH8_W  !ByteOff
   | PUSH16_W !ByteOff
   | PUSH32_W !ByteOff

   -- Push a (heap) ptr  (these all map to PUSH_G really)
   | PUSH_G       Name
   | PUSH_PRIMOP  PrimOp
   | PUSH_BCO     (ProtoBCO Name)

   -- Push an alt continuation
   | PUSH_ALTS          (ProtoBCO Name) ArgRep
   | PUSH_ALTS_TUPLE    (ProtoBCO Name) -- continuation
                        !NativeCallInfo
                        (ProtoBCO Name) -- tuple return BCO

   -- Pushing 8, 16 and 32 bits of padding (for constructors).
   | PUSH_PAD8
   | PUSH_PAD16
   | PUSH_PAD32

   -- Pushing literals
   | PUSH_UBX8  Literal
   | PUSH_UBX16 Literal
   | PUSH_UBX32 Literal
   | PUSH_UBX   Literal !WordOff
        -- push this int/float/double/addr, on the stack. Word
        -- is # of words to copy from literal pool.  Eitherness reflects
        -- the difficulty of dealing with MachAddr here, mostly due to
        -- the excessive (and unnecessary) restrictions imposed by the
        -- designers of the new Foreign library.  In particular it is
        -- quite impossible to convert an Addr to any other integral
        -- type, and it appears impossible to get hold of the bits of
        -- an addr, even though we need to assemble BCOs.

   -- Push a top-level Addr#. This is a pseudo-instruction assembled to PUSH_UBX,
   -- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode.
   | PUSH_ADDR Name

   -- various kinds of application
   | PUSH_APPLY_N
   | PUSH_APPLY_V
   | PUSH_APPLY_F
   | PUSH_APPLY_D
   | PUSH_APPLY_L
   | PUSH_APPLY_P
   | PUSH_APPLY_PP
   | PUSH_APPLY_PPP
   | PUSH_APPLY_PPPP
   | PUSH_APPLY_PPPPP
   | PUSH_APPLY_PPPPPP

   -- | Drop entries @(n, n+by]@ entries from the stack. Graphically:
   -- @
   -- a_1  ← top
   -- ...
   -- a_n
   -- b_1              =>    a_1  ← top
   -- ...                    ...
   -- b_by                   a_n
   -- k                      k
   -- @
   | SLIDE     !WordOff -- ^ n = this many
               !WordOff -- ^ by = down by this much

   -- To do with the heap
   | ALLOC_AP  !HalfWord {- make an AP with this many payload words.
                            HalfWord matches the size of the n_args field in StgAP,
                            make sure that we handle truncation when generating
                            bytecode using this HalfWord type here -}
   | ALLOC_AP_NOUPD !HalfWord -- make an AP_NOUPD with this many payload words
   | ALLOC_PAP !HalfWord !HalfWord -- make a PAP with this arity / payload words
   | MKAP      !WordOff{-ptr to AP is this far down stack-} !HalfWord{-number of words-}
   | MKPAP     !WordOff{-ptr to PAP is this far down stack-} !HalfWord{-number of words-}
   | UNPACK    !WordOff -- unpack N words from t.o.s Constr
   | PACK      DataCon !WordOff
                        -- after assembly, the DataCon is an index into the
                        -- itbl array
   -- For doing case trees
   | LABEL     LocalLabel
   | TESTLT_I   !Int    LocalLabel
   | TESTEQ_I   !Int    LocalLabel
   | TESTLT_W   !Word   LocalLabel
   | TESTEQ_W   !Word   LocalLabel
   | TESTLT_I64 !Int64  LocalLabel
   | TESTEQ_I64 !Int64  LocalLabel
   | TESTLT_I32 !Int32  LocalLabel
   | TESTEQ_I32 !Int32  LocalLabel
   | TESTLT_I16 !Int16  LocalLabel
   | TESTEQ_I16 !Int16  LocalLabel
   | TESTLT_I8  !Int8   LocalLabel
   | TESTEQ_I8  !Int16  LocalLabel
   | TESTLT_W64 !Word64 LocalLabel
   | TESTEQ_W64 !Word64 LocalLabel
   | TESTLT_W32 !Word32 LocalLabel
   | TESTEQ_W32 !Word32 LocalLabel
   | TESTLT_W16 !Word16 LocalLabel
   | TESTEQ_W16 !Word16 LocalLabel
   | TESTLT_W8  !Word8  LocalLabel
   | TESTEQ_W8  !Word8  LocalLabel
   | TESTLT_F   !Float  LocalLabel
   | TESTEQ_F   !Float  LocalLabel
   | TESTLT_D   !Double LocalLabel
   | TESTEQ_D   !Double LocalLabel

   -- The Word16 value is a constructor number and therefore
   -- stored in the insn stream rather than as an offset into
   -- the literal pool.

   -- | Test whether the tag of a closure pointer is less than the given value.
   -- If not, jump to the given label.
   | TESTLT_P  !Word16 LocalLabel
   -- | Test whether the tag of a closure pointer is equal to the given value.
   -- If not, jump to the given label.
   | TESTEQ_P  !Word16 LocalLabel

   | CASEFAIL
   | JMP              LocalLabel

   -- For doing calls to C (via glue code generated by libffi)
   | CCALL            !WordOff  -- stack frame size
                      (RemotePtr C_ffi_cif) -- addr of the glue code
                      !Word16   -- flags.
                                --
                                -- 0x1: call is interruptible
                                -- 0x2: call is unsafe
                                --
                                -- (XXX: inefficient, but I don't know
                                -- what the alignment constraints are.)

   | PRIMCALL

   -- For doing magic ByteArray passing to foreign calls
   | SWIZZLE          !WordOff -- to the ptr N words down the stack,
                      !Int     -- add M

   -- To Infinity And Beyond
   | ENTER
   | RETURN ArgRep -- return a non-tuple value, here's its rep; see
                   -- Note [Return convention for non-tuple values] in GHC.StgToByteCode
   | RETURN_TUPLE  -- return an unboxed tuple (info already on stack); see
                   -- Note [unboxed tuple bytecodes and tuple_BCO] in GHC.StgToByteCode

   -- Breakpoints
   | BRK_FUN          (ForeignRef BreakArray)
                      (RemotePtr ModuleName) -- breakpoint tick module
                      (RemotePtr UnitId)     -- breakpoint tick module unit id
                      !Word16                -- breakpoint tick index
                      (RemotePtr ModuleName) -- breakpoint info module
                      (RemotePtr UnitId)     -- breakpoint info module unit id
                      !Word16                -- breakpoint info index
                      (RemotePtr CostCentre)

#if MIN_VERSION_rts(1,0,3)
   -- | A "meta"-instruction for recording the name of a BCO for debugging purposes.
   -- These are ignored by the interpreter but helpfully printed by the disassmbler.
   | BCO_NAME         !ByteString
#endif


{- Note [BCO_NAME]
   ~~~~~~~~~~~~~~~
   The BCO_NAME instruction is a debugging-aid enabled with the -fadd-bco-name flag.
   When enabled the bytecode assembler will prepend a BCO_NAME instruction to every
   generated bytecode object capturing the STG name of the binding the BCO implements.
   This is then printed by the bytecode disassembler, allowing bytecode objects to be
   readily correlated with their STG and Core source.
 -}

-- -----------------------------------------------------------------------------
-- Printing bytecode instructions

instance Outputable a => Outputable (ProtoBCO a) where
   ppr (ProtoBCO { protoBCOName       = name
                 , protoBCOInstrs     = instrs
                 , protoBCOBitmap     = bitmap
                 , protoBCOBitmapSize = bsize
                 , protoBCOArity      = arity
                 , protoBCOExpr       = origin
                 , protoBCOFFIs       = ffis })
      = (text "ProtoBCO" <+> ppr name <> char '#' <> int arity
                <+> text (show ffis) <> colon)
        $$ nest 3 (case origin of
                      Left alts ->
                        vcat (zipWith (<+>) (char '{' : repeat (char ';'))
                             (map (pprStgAltShort shortStgPprOpts) alts))
                      Right rhs ->
                        pprStgRhsShort shortStgPprOpts rhs
                  )
        $$ nest 3 (text "bitmap: " <+> text (show bsize) <+> ppr bitmap)
        $$ nest 3 (vcat (map ppr instrs))

-- Print enough of the STG expression to enable the reader to find
-- the expression in the -ddump-stg output.  That is, we need to
-- include at least a binder.

pprStgExprShort :: OutputablePass pass => StgPprOpts -> GenStgExpr pass -> SDoc
pprStgExprShort _ (StgCase _expr var _ty _alts) =
  text "case of" <+> ppr var
pprStgExprShort _ (StgLet _ bnd _) =
  text "let" <+> pprStgBindShort bnd <+> text "in ..."
pprStgExprShort _ (StgLetNoEscape _ bnd _) =
  text "let-no-escape" <+> pprStgBindShort bnd <+> text "in ..."
pprStgExprShort opts (StgTick t e) = ppr t <+> pprStgExprShort opts e
pprStgExprShort opts e = pprStgExpr opts e

pprStgBindShort :: OutputablePass pass => GenStgBinding pass -> SDoc
pprStgBindShort (StgNonRec x _) =
  ppr x <+> text "= ..."
pprStgBindShort (StgRec bs) =
  char '{' <+> ppr (fst (head bs)) <+> text "= ...; ... }"

pprStgAltShort :: OutputablePass pass => StgPprOpts -> GenStgAlt pass -> SDoc
pprStgAltShort opts GenStgAlt{alt_con=con, alt_bndrs=args, alt_rhs=expr} =
  ppr con <+> sep (map ppr args) <+> text "->" <+> pprStgExprShort opts expr

pprStgRhsShort :: OutputablePass pass => StgPprOpts -> GenStgRhs pass -> SDoc
pprStgRhsShort opts (StgRhsClosure _ext _cc upd_flag args body _typ) =
  hang (hsep [ char '\\' <> ppr upd_flag, brackets (interppSP args) ])
       4 (pprStgExprShort opts body)
pprStgRhsShort opts rhs = pprStgRhs opts rhs


instance Outputable BCInstr where
   ppr (STKCHECK n)          = text "STKCHECK" <+> ppr n
   ppr (PUSH_L offset)       = text "PUSH_L  " <+> ppr offset
   ppr (PUSH_LL o1 o2)       = text "PUSH_LL " <+> ppr o1 <+> ppr o2
   ppr (PUSH_LLL o1 o2 o3)   = text "PUSH_LLL" <+> ppr o1 <+> ppr o2 <+> ppr o3
   ppr (PUSH8  offset)       = text "PUSH8  " <+> ppr offset
   ppr (PUSH16 offset)       = text "PUSH16  " <+> ppr offset
   ppr (PUSH32 offset)       = text "PUSH32  " <+> ppr offset
   ppr (PUSH8_W  offset)     = text "PUSH8_W  " <+> ppr offset
   ppr (PUSH16_W offset)     = text "PUSH16_W  " <+> ppr offset
   ppr (PUSH32_W offset)     = text "PUSH32_W  " <+> ppr offset
   ppr (PUSH_G nm)           = text "PUSH_G  " <+> ppr nm
   ppr (PUSH_PRIMOP op)      = text "PUSH_G  " <+> text "GHC.PrimopWrappers."
                                               <> ppr op
   ppr (PUSH_BCO bco)        = hang (text "PUSH_BCO") 2 (ppr bco)

   ppr (PUSH_ALTS bco pk)    = hang (text "PUSH_ALTS" <+> ppr pk) 2 (ppr bco)
   ppr (PUSH_ALTS_TUPLE bco call_info tuple_bco) =
                               hang (text "PUSH_ALTS_TUPLE" <+> ppr call_info)
                                    2
                                    (ppr tuple_bco $+$ ppr bco)

   ppr PUSH_PAD8             = text "PUSH_PAD8"
   ppr PUSH_PAD16            = text "PUSH_PAD16"
   ppr PUSH_PAD32            = text "PUSH_PAD32"

   ppr (PUSH_UBX8  lit)      = text "PUSH_UBX8" <+> ppr lit
   ppr (PUSH_UBX16 lit)      = text "PUSH_UBX16" <+> ppr lit
   ppr (PUSH_UBX32 lit)      = text "PUSH_UBX32" <+> ppr lit
   ppr (PUSH_UBX lit nw)     = text "PUSH_UBX" <+> parens (ppr nw) <+> ppr lit
   ppr (PUSH_ADDR nm)        = text "PUSH_ADDR" <+> ppr nm
   ppr PUSH_APPLY_N          = text "PUSH_APPLY_N"
   ppr PUSH_APPLY_V          = text "PUSH_APPLY_V"
   ppr PUSH_APPLY_F          = text "PUSH_APPLY_F"
   ppr PUSH_APPLY_D          = text "PUSH_APPLY_D"
   ppr PUSH_APPLY_L          = text "PUSH_APPLY_L"
   ppr PUSH_APPLY_P          = text "PUSH_APPLY_P"
   ppr PUSH_APPLY_PP         = text "PUSH_APPLY_PP"
   ppr PUSH_APPLY_PPP        = text "PUSH_APPLY_PPP"
   ppr PUSH_APPLY_PPPP       = text "PUSH_APPLY_PPPP"
   ppr PUSH_APPLY_PPPPP      = text "PUSH_APPLY_PPPPP"
   ppr PUSH_APPLY_PPPPPP     = text "PUSH_APPLY_PPPPPP"

   ppr (SLIDE n d)           = text "SLIDE   " <+> ppr n <+> ppr d
   ppr (ALLOC_AP sz)         = text "ALLOC_AP   " <+> ppr sz
   ppr (ALLOC_AP_NOUPD sz)   = text "ALLOC_AP_NOUPD   " <+> ppr sz
   ppr (ALLOC_PAP arity sz)  = text "ALLOC_PAP   " <+> ppr arity <+> ppr sz
   ppr (MKAP offset sz)      = text "MKAP    " <+> ppr sz <+> text "words,"
                                               <+> ppr offset <+> text "stkoff"
   ppr (MKPAP offset sz)     = text "MKPAP   " <+> ppr sz <+> text "words,"
                                               <+> ppr offset <+> text "stkoff"
   ppr (UNPACK sz)           = text "UNPACK  " <+> ppr sz
   ppr (PACK dcon sz)        = text "PACK    " <+> ppr dcon <+> ppr sz
   ppr (LABEL     lab)       = text "__"       <> ppr lab <> colon
   ppr (TESTLT_I  i lab)     = text "TESTLT_I" <+> int i <+> text "__" <> ppr lab
   ppr (TESTEQ_I  i lab)     = text "TESTEQ_I" <+> int i <+> text "__" <> ppr lab
   ppr (TESTLT_W  i lab)     = text "TESTLT_W" <+> int (fromIntegral i) <+> text "__" <> ppr lab
   ppr (TESTEQ_W  i lab)     = text "TESTEQ_W" <+> int (fromIntegral i) <+> text "__" <> ppr lab
   ppr (TESTLT_I64  i lab)   = text "TESTLT_I64" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_I64  i lab)   = text "TESTEQ_I64" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_I32  i lab)   = text "TESTLT_I32" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_I32  i lab)   = text "TESTEQ_I32" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_I16  i lab)   = text "TESTLT_I16" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_I16  i lab)   = text "TESTEQ_I16" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_I8  i lab)    = text "TESTLT_I8" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_I8  i lab)    = text "TESTEQ_I8" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_W64  i lab)   = text "TESTLT_W64" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_W64  i lab)   = text "TESTEQ_W64" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_W32  i lab)   = text "TESTLT_W32" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_W32  i lab)   = text "TESTEQ_W32" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_W16  i lab)   = text "TESTLT_W16" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_W16  i lab)   = text "TESTEQ_W16" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_W8  i lab)    = text "TESTLT_W8" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_W8  i lab)    = text "TESTEQ_W8" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTLT_F  f lab)     = text "TESTLT_F" <+> float f <+> text "__" <> ppr lab
   ppr (TESTEQ_F  f lab)     = text "TESTEQ_F" <+> float f <+> text "__" <> ppr lab
   ppr (TESTLT_D  d lab)     = text "TESTLT_D" <+> double d <+> text "__" <> ppr lab
   ppr (TESTEQ_D  d lab)     = text "TESTEQ_D" <+> double d <+> text "__" <> ppr lab
   ppr (TESTLT_P  i lab)     = text "TESTLT_P" <+> ppr i <+> text "__" <> ppr lab
   ppr (TESTEQ_P  i lab)     = text "TESTEQ_P" <+> ppr i <+> text "__" <> ppr lab
   ppr CASEFAIL              = text "CASEFAIL"
   ppr (JMP lab)             = text "JMP"      <+> ppr lab
   ppr (CCALL off marshal_addr flags) = text "CCALL   " <+> ppr off
                                                <+> text "marshal code at"
                                               <+> text (show marshal_addr)
                                               <+> (case flags of
                                                      0x1 -> text "(interruptible)"
                                                      0x2 -> text "(unsafe)"
                                                      _   -> empty)
   ppr PRIMCALL              = text "PRIMCALL"
   ppr (SWIZZLE stkoff n)    = text "SWIZZLE " <+> text "stkoff" <+> ppr stkoff
                                               <+> text "by" <+> ppr n
   ppr ENTER                 = text "ENTER"
   ppr (RETURN pk)           = text "RETURN  " <+> ppr pk
   ppr (RETURN_TUPLE)        = text "RETURN_TUPLE"
   ppr (BRK_FUN _ _tick_mod _tick_mod_id tickx _info_mod _info_mod_id infox _)
                             = text "BRK_FUN" <+> text "<breakarray>"
                               <+> text "<tick_module>" <+> text "<tick_module_unitid>" <+> ppr tickx
                               <+> text "<info_module>" <+> text "<info_module_unitid>" <+> ppr infox
                               <+> text "<cc>"
#if MIN_VERSION_rts(1,0,3)
   ppr (BCO_NAME nm)         = text "BCO_NAME" <+> text (show nm)
#endif



-- -----------------------------------------------------------------------------
-- The stack use, in words, of each bytecode insn.  These _must_ be
-- correct, or overestimates of reality, to be safe.

-- NOTE: we aggregate the stack use from case alternatives too, so that
-- we can do a single stack check at the beginning of a function only.

-- This could all be made more accurate by keeping track of a proper
-- stack high water mark, but it doesn't seem worth the hassle.

protoBCOStackUse :: ProtoBCO a -> Word
protoBCOStackUse bco = sum (map bciStackUse (protoBCOInstrs bco))

bciStackUse :: BCInstr -> Word
bciStackUse STKCHECK{}            = 0
bciStackUse PUSH_L{}              = 1
bciStackUse PUSH_LL{}             = 2
bciStackUse PUSH_LLL{}            = 3
bciStackUse PUSH8{}               = 1  -- overapproximation
bciStackUse PUSH16{}              = 1  -- overapproximation
bciStackUse PUSH32{}              = 1  -- overapproximation on 64bit arch
bciStackUse PUSH8_W{}             = 1  -- takes exactly 1 word
bciStackUse PUSH16_W{}            = 1  -- takes exactly 1 word
bciStackUse PUSH32_W{}            = 1  -- takes exactly 1 word
bciStackUse PUSH_G{}              = 1
bciStackUse PUSH_PRIMOP{}         = 1
bciStackUse PUSH_BCO{}            = 1
bciStackUse (PUSH_ALTS bco _)     = 2 {- profiling only, restore CCCS -} +
                                    3 + protoBCOStackUse bco
bciStackUse (PUSH_ALTS_TUPLE bco info _) =
   -- (tuple_bco, call_info word, cont_bco, stg_ctoi_t)
   -- tuple
   -- (call_info, tuple_bco, stg_ret_t)
   1 {- profiling only -} +
   7 + fromIntegral (nativeCallSize info) + protoBCOStackUse bco
bciStackUse (PUSH_PAD8)           = 1  -- overapproximation
bciStackUse (PUSH_PAD16)          = 1  -- overapproximation
bciStackUse (PUSH_PAD32)          = 1  -- overapproximation on 64bit arch
bciStackUse (PUSH_UBX8 _)         = 1  -- overapproximation
bciStackUse (PUSH_UBX16 _)        = 1  -- overapproximation
bciStackUse (PUSH_UBX32 _)        = 1  -- overapproximation on 64bit arch
bciStackUse (PUSH_UBX _ nw)       = fromIntegral nw
bciStackUse PUSH_ADDR{}           = 1
bciStackUse PUSH_APPLY_N{}        = 1
bciStackUse PUSH_APPLY_V{}        = 1
bciStackUse PUSH_APPLY_F{}        = 1
bciStackUse PUSH_APPLY_D{}        = 1
bciStackUse PUSH_APPLY_L{}        = 1
bciStackUse PUSH_APPLY_P{}        = 1
bciStackUse PUSH_APPLY_PP{}       = 1
bciStackUse PUSH_APPLY_PPP{}      = 1
bciStackUse PUSH_APPLY_PPPP{}     = 1
bciStackUse PUSH_APPLY_PPPPP{}    = 1
bciStackUse PUSH_APPLY_PPPPPP{}   = 1
bciStackUse ALLOC_AP{}            = 1
bciStackUse ALLOC_AP_NOUPD{}      = 1
bciStackUse ALLOC_PAP{}           = 1
bciStackUse (UNPACK sz)           = fromIntegral sz
bciStackUse LABEL{}               = 0
bciStackUse TESTLT_I{}            = 0
bciStackUse TESTEQ_I{}            = 0
bciStackUse TESTLT_W{}            = 0
bciStackUse TESTEQ_W{}            = 0
bciStackUse TESTLT_I64{}          = 0
bciStackUse TESTEQ_I64{}          = 0
bciStackUse TESTLT_I32{}          = 0
bciStackUse TESTEQ_I32{}          = 0
bciStackUse TESTLT_I16{}          = 0
bciStackUse TESTEQ_I16{}          = 0
bciStackUse TESTLT_I8{}           = 0
bciStackUse TESTEQ_I8{}           = 0
bciStackUse TESTLT_W64{}          = 0
bciStackUse TESTEQ_W64{}          = 0
bciStackUse TESTLT_W32{}          = 0
bciStackUse TESTEQ_W32{}          = 0
bciStackUse TESTLT_W16{}          = 0
bciStackUse TESTEQ_W16{}          = 0
bciStackUse TESTLT_W8{}           = 0
bciStackUse TESTEQ_W8{}           = 0
bciStackUse TESTLT_F{}            = 0
bciStackUse TESTEQ_F{}            = 0
bciStackUse TESTLT_D{}            = 0
bciStackUse TESTEQ_D{}            = 0
bciStackUse TESTLT_P{}            = 0
bciStackUse TESTEQ_P{}            = 0
bciStackUse CASEFAIL{}            = 0
bciStackUse JMP{}                 = 0
bciStackUse ENTER{}               = 0
bciStackUse RETURN{}              = 1 -- pushes stg_ret_X for some X
bciStackUse RETURN_TUPLE{}        = 1 -- pushes stg_ret_t header
bciStackUse CCALL{}               = 0
bciStackUse PRIMCALL{}            = 1 -- pushes stg_primcall
bciStackUse SWIZZLE{}             = 0
bciStackUse BRK_FUN{}             = 0

-- These insns actually reduce stack use, but we need the high-tide level,
-- so can't use this info.  Not that it matters much.
bciStackUse SLIDE{}               = 0
bciStackUse MKAP{}                = 0
bciStackUse MKPAP{}               = 0
bciStackUse PACK{}                = 1 -- worst case is PACK 0 words
#if MIN_VERSION_rts(1,0,3)
bciStackUse BCO_NAME{}            = 0
#endif

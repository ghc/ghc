{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.Ppr (
        doubleToBytes,
        floatToBytes,
        pprString,
        pprFileEmbed,
        pprSectionHeader
)

where

import GHC.Prelude

import GHC.Utils.Asm
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.CmmToAsm.Config
import GHC.Utils.Outputable as SDoc
import qualified GHC.Utils.Ppr as Pretty
import GHC.Utils.Panic
import GHC.Utils.Misc ( charToC )
import GHC.Platform

import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

import Control.Monad.ST
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Exts
import GHC.Word

#if !MIN_VERSION_base(4,16,0)
word8ToWord# :: Word# -> Word#
word8ToWord# w = w
{-# INLINE word8ToWord# #-}
#endif

-- -----------------------------------------------------------------------------
-- Converting floating-point literals to integrals for printing

-- | Get bytes of a Float representation
floatToBytes :: Float -> [Word8]
floatToBytes f = runST $ do
  arr <- newArray_ ((0::Int),3)
  writeArray arr 0 f
  let cast :: STUArray s Int Float -> ST s (STUArray s Int Word8)
      cast = U.castSTUArray
  arr <- cast arr
  i0 <- readArray arr 0
  i1 <- readArray arr 1
  i2 <- readArray arr 2
  i3 <- readArray arr 3
  return [i0,i1,i2,i3]

-- | Get bytes of a Double representation
doubleToBytes :: Double -> [Word8]
doubleToBytes d = runST $ do
  arr <- newArray_ ((0::Int),7)
  writeArray arr 0 d
  let cast :: STUArray s Int Double -> ST s (STUArray s Int Word8)
      cast = U.castSTUArray
  arr <- cast arr
  i0 <- readArray arr 0
  i1 <- readArray arr 1
  i2 <- readArray arr 2
  i3 <- readArray arr 3
  i4 <- readArray arr 4
  i5 <- readArray arr 5
  i6 <- readArray arr 6
  i7 <- readArray arr 7
  return [i0,i1,i2,i3,i4,i5,i6,i7]


-- ---------------------------------------------------------------------------
-- Printing ASCII strings.
--
-- Print as a string and escape non-printable characters.

-- | Emit a ".string" directive
pprString :: ByteString -> SDoc
pprString bs = text "\t.string " <> doubleQuotes (text (concatMap charToC (BS.unpack bs)))

-- | Emit a ".incbin" directive
--
-- A NULL byte is added after the binary data.
pprFileEmbed :: FilePath -> SDoc
pprFileEmbed path
   = text "\t.incbin "
     <> pprFilePathString path -- proper escape (see #16389)
     <> text "\n\t.byte 0"

{-
Note [Embedding large binary blobs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To embed a blob of binary data (e.g. an UTF-8 encoded string) into the generated
code object, we have several options:

   1. Generate a ".byte" directive for each byte. This is what was done in the past
      (see Note [Pretty print ASCII when AsmCodeGen]).

   2. Generate a single ".string"/".asciz" directive for the whole sequence of
      bytes. Bytes in the ASCII printable range are rendered as characters and
      other values are escaped (e.g., "\t", "\077", etc.).

   3. Create a temporary file into which we dump the binary data and generate a
      single ".incbin" directive. The assembler will include the binary file for
      us in the generated output object.

Now the code generator uses either (2) or (3), depending on the binary blob
size.  Using (3) for small blobs adds too much overhead (see benchmark results
in #16190), so we only do it when the size is above a threshold (500K at the
time of writing).

The threshold is configurable via the `-fbinary-blob-threshold` flag.

-}

-- ----------------------------------------------------------------------------
-- Printing section headers.
--
-- If -split-section was specified, include the suffix label, otherwise just
-- print the section type. For Darwin, where subsections-for-symbols are
-- used instead, only print section type.
--
-- For string literals, additional flags are specified to enable merging of
-- identical strings in the linker. With -split-sections each string also gets
-- a unique section to allow strings from unused code to be GC'd.

pprSectionHeader :: NCGConfig -> Section -> SDoc
pprSectionHeader config (Section t suffix) =
 case platformOS (ncgPlatform config) of
   OSAIX     -> pprXcoffSectionHeader t
   OSDarwin  -> pprDarwinSectionHeader t
   _         -> pprGNUSectionHeader config t suffix

pprGNUSectionHeader :: NCGConfig -> SectionType -> CLabel -> SDoc
pprGNUSectionHeader config t suffix =
  hcat [text ".section ", header, subsection, flags]
  where
    sep
      | OSMinGW32 <- platformOS platform = char '$'
      | otherwise                        = char '.'
    platform      = ncgPlatform config
    splitSections = ncgSplitSections config
    subsection
      | splitSections = sep <> pdoc platform suffix
      | otherwise     = empty
    header = case t of
      Text -> text ".text"
      Data -> text ".data"
      ReadOnlyData  | OSMinGW32 <- platformOS platform
                                -> text ".rdata"
                    | otherwise -> text ".rodata"
      RelocatableReadOnlyData | OSMinGW32 <- platformOS platform
                                -- Concept does not exist on Windows,
                                -- So map these to R/O data.
                                          -> text ".rdata$rel.ro"
                              | otherwise -> text ".data.rel.ro"
      UninitialisedData -> text ".bss"
      ReadOnlyData16 | OSMinGW32 <- platformOS platform
                                 -> text ".rdata$cst16"
                     | otherwise -> text ".rodata.cst16"
      InitArray
        | OSMinGW32 <- platformOS platform
                    -> text ".ctors"
        | otherwise -> text ".init_array"
      FiniArray
        | OSMinGW32 <- platformOS platform
                    -> text ".dtors"
        | otherwise -> text ".fini_array"
      CString
        | OSMinGW32 <- platformOS platform
                    -> text ".rdata"
        | otherwise -> text ".rodata.str"
      OtherSection _ ->
        panic "PprBase.pprGNUSectionHeader: unknown section type"
    flags = case t of
      CString
        | OSMinGW32 <- platformOS platform
                    -> empty
        | otherwise -> text ",\"aMS\"," <> sectionType platform "progbits" <> text ",1"
      _ -> empty

-- XCOFF doesn't support relocating label-differences, so we place all
-- RO sections into .text[PR] sections
pprXcoffSectionHeader :: SectionType -> SDoc
pprXcoffSectionHeader t = case t of
  Text                    -> text ".csect .text[PR]"
  Data                    -> text ".csect .data[RW]"
  ReadOnlyData            -> text ".csect .text[PR] # ReadOnlyData"
  RelocatableReadOnlyData -> text ".csect .text[PR] # RelocatableReadOnlyData"
  ReadOnlyData16          -> text ".csect .text[PR] # ReadOnlyData16"
  CString                 -> text ".csect .text[PR] # CString"
  UninitialisedData       -> text ".csect .data[BS]"
  _                       -> panic "pprXcoffSectionHeader: unknown section type"

pprDarwinSectionHeader :: SectionType -> SDoc
pprDarwinSectionHeader t = case t of
  Text                    -> text ".text"
  Data                    -> text ".data"
  ReadOnlyData            -> text ".const"
  RelocatableReadOnlyData -> text ".const_data"
  UninitialisedData       -> text ".data"
  ReadOnlyData16          -> text ".const"
  InitArray               -> text ".section\t__DATA,__mod_init_func,mod_init_funcs"
  FiniArray               -> panic "pprDarwinSectionHeader: fini not supported"
  CString                 -> text ".section\t__TEXT,__cstring,cstring_literals"
  OtherSection _          -> panic "pprDarwinSectionHeader: unknown section type"

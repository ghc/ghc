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
        pprASCII,
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
import GHC.Utils.Panic
import GHC.Platform

import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

import Control.Monad.ST
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Exts
import GHC.Word


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
-- This is similar to charToC in GHC.Utils.Misc

pprASCII :: forall doc. IsLine doc => ByteString -> doc
pprASCII str
  -- Transform this given literal bytestring to escaped string and construct
  -- the literal SDoc directly.
  -- See #14741
  -- and Note [Pretty print ASCII when AsmCodeGen]
  --
  -- We work with a `Doc` instead of an `SDoc` because there is no need to carry
  -- an `SDocContext` that we don't use. It leads to nicer (STG) code.
  = BS.foldr f empty str
    where
       f :: Word8 -> doc -> doc
       f w s = do1 w <> s

       do1 :: Word8 -> doc
       do1 w | 0x09 == w = text "\\t"
             | 0x0A == w = text "\\n"
             | 0x22 == w = text "\\\""
             | 0x5C == w = text "\\\\"
               -- ASCII printable characters range
             | w >= 0x20 && w <= 0x7E = char (chr' w)
             | otherwise = text xs
                where
                 !xs = [ '\\', x0, x1, x2] -- octal
                 !x0 = chr' (ord0 + (w `unsafeShiftR` 6) .&. 0x07)
                 !x1 = chr' (ord0 + (w `unsafeShiftR` 3) .&. 0x07)
                 !x2 = chr' (ord0 + w .&. 0x07)
                 !ord0 = 0x30 -- = ord '0'

       -- we know that the Chars we create are in the ASCII range
       -- so we bypass the check in "chr"
       chr' :: Word8 -> Char
       chr' (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)))
{-# SPECIALIZE pprASCII :: ByteString -> SDoc #-}
{-# SPECIALIZE pprASCII :: ByteString -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Emit a ".string" directive
pprString :: IsLine doc => ByteString -> doc
pprString bs = text "\t.string " <> doubleQuotes (pprASCII bs)
{-# SPECIALIZE pprString :: ByteString -> SDoc #-}
{-# SPECIALIZE pprString :: ByteString -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Emit a ".incbin" directive
--
-- A NULL byte is added after the binary data.
pprFileEmbed :: IsLine doc => FilePath -> doc
pprFileEmbed path
   = text "\t.incbin "
     <> pprFilePathString path -- proper escape (see #16389)
     <> text "\n\t.byte 0"
{-# SPECIALIZE pprFileEmbed :: FilePath -> SDoc #-}
{-# SPECIALIZE pprFileEmbed :: FilePath -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

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


{-
Note [Pretty print ASCII when AsmCodeGen]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously, when generating assembly code, we created SDoc with
`(ptext . sLit)` for every bytes in literal bytestring, then
combine them using `hcat`.

When handling literal bytestrings with millions of bytes,
millions of SDoc would be created and to combine, leading to
high memory usage.

Now we escape the given bytestring to string directly and construct
SDoc only once. This improvement could dramatically decrease the
memory allocation from 4.7GB to 1.3GB when embedding a 3MB literal
string in source code. See #14741 for profiling results.
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

pprSectionHeader :: IsLine doc => NCGConfig -> Section -> doc
pprSectionHeader config (Section t suffix) =
 case platformOS (ncgPlatform config) of
   OSAIX     -> pprXcoffSectionHeader t
   OSDarwin  -> pprDarwinSectionHeader t
   _         -> pprGNUSectionHeader config t suffix
{-# SPECIALIZE pprSectionHeader :: NCGConfig -> Section -> SDoc #-}
{-# SPECIALIZE pprSectionHeader :: NCGConfig -> Section -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

pprGNUSectionHeader :: IsLine doc => NCGConfig -> SectionType -> CLabel -> doc
pprGNUSectionHeader config t suffix =
  hcat [text ".section ", header, subsection, flags]
  where
    sep
      | OSMinGW32 <- platformOS platform = char '$'
      | otherwise                        = char '.'
    platform      = ncgPlatform config
    splitSections = ncgSplitSections config
    subsection
      | splitSections = sep <> pprAsmLabel platform suffix
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
      Text
        | OSMinGW32 <- platformOS platform, splitSections
                    -> text ",\"xr\""
        | splitSections
                    -> text ",\"ax\"," <> sectionType platform "progbits"
      CString
        | OSMinGW32 <- platformOS platform
                    -> empty
        | otherwise -> text ",\"aMS\"," <> sectionType platform "progbits" <> text ",1"
      _ -> empty
{-# SPECIALIZE pprGNUSectionHeader :: NCGConfig -> SectionType -> CLabel -> SDoc #-}
{-# SPECIALIZE pprGNUSectionHeader :: NCGConfig -> SectionType -> CLabel -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- XCOFF doesn't support relocating label-differences, so we place all
-- RO sections into .text[PR] sections
pprXcoffSectionHeader :: IsLine doc => SectionType -> doc
pprXcoffSectionHeader t = case t of
  Text                    -> text ".csect .text[PR]"
  Data                    -> text ".csect .data[RW]"
  ReadOnlyData            -> text ".csect .text[PR] # ReadOnlyData"
  RelocatableReadOnlyData -> text ".csect .text[PR] # RelocatableReadOnlyData"
  CString                 -> text ".csect .text[PR] # CString"
  UninitialisedData       -> text ".csect .data[BS]"
  _                       -> panic "pprXcoffSectionHeader: unknown section type"
{-# SPECIALIZE pprXcoffSectionHeader :: SectionType -> SDoc #-}
{-# SPECIALIZE pprXcoffSectionHeader :: SectionType -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

pprDarwinSectionHeader :: IsLine doc => SectionType -> doc
pprDarwinSectionHeader t = case t of
  Text                    -> text ".text"
  Data                    -> text ".data"
  ReadOnlyData            -> text ".const"
  RelocatableReadOnlyData -> text ".const_data"
  UninitialisedData       -> text ".data"
  InitArray               -> text ".section\t__DATA,__mod_init_func,mod_init_funcs"
  FiniArray               -> panic "pprDarwinSectionHeader: fini not supported"
  CString                 -> text ".section\t__TEXT,__cstring,cstring_literals"
  OtherSection _          -> panic "pprDarwinSectionHeader: unknown section type"
{-# SPECIALIZE pprDarwinSectionHeader :: SectionType -> SDoc #-}
{-# SPECIALIZE pprDarwinSectionHeader :: SectionType -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

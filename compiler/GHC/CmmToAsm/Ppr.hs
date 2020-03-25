{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.Ppr (
        castFloatToWord8Array,
        castDoubleToWord8Array,
        floatToBytes,
        doubleToBytes,
        pprASCII,
        pprString,
        pprFileEmbed,
        pprSectionHeader
)

where

import GhcPrelude

import AsmUtils
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.CmmToAsm.Config
import FastString
import Outputable
import GHC.Platform

import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

import Control.Monad.ST

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Exts
import GHC.Word



-- -----------------------------------------------------------------------------
-- Converting floating-point literals to integrals for printing

castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToWord8Array = U.castSTUArray

castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToWord8Array = U.castSTUArray

-- floatToBytes and doubleToBytes convert to the host's byte
-- order.  Providing that we're not cross-compiling for a
-- target with the opposite endianness, this should work ok
-- on all targets.

-- ToDo: this stuff is very similar to the shenanigans in PprAbs,
-- could they be merged?

floatToBytes :: Float -> [Int]
floatToBytes f
   = runST (do
        arr <- newArray_ ((0::Int),3)
        writeArray arr 0 f
        arr <- castFloatToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        return (map fromIntegral [i0,i1,i2,i3])
     )

doubleToBytes :: Double -> [Int]
doubleToBytes d
   = runST (do
        arr <- newArray_ ((0::Int),7)
        writeArray arr 0 d
        arr <- castDoubleToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        i4 <- readArray arr 4
        i5 <- readArray arr 5
        i6 <- readArray arr 6
        i7 <- readArray arr 7
        return (map fromIntegral [i0,i1,i2,i3,i4,i5,i6,i7])
     )

-- ---------------------------------------------------------------------------
-- Printing ASCII strings.
--
-- Print as a string and escape non-printable characters.
-- This is similar to charToC in Utils.

pprASCII :: ByteString -> SDoc
pprASCII str
  -- Transform this given literal bytestring to escaped string and construct
  -- the literal SDoc directly.
  -- See #14741
  -- and Note [Pretty print ASCII when AsmCodeGen]
  = text $ BS.foldr (\w s -> do1 w ++ s) "" str
    where
       do1 :: Word8 -> String
       do1 w | 0x09 == w = "\\t"
             | 0x0A == w = "\\n"
             | 0x22 == w = "\\\""
             | 0x5C == w = "\\\\"
               -- ASCII printable characters range
             | w >= 0x20 && w <= 0x7E = [chr' w]
             | otherwise = '\\' : octal w

       -- we know that the Chars we create are in the ASCII range
       -- so we bypass the check in "chr"
       chr' :: Word8 -> Char
       chr' (W8# w#) = C# (chr# (word2Int# w#))

       octal :: Word8 -> String
       octal w = [ chr' (ord0 + (w `unsafeShiftR` 6) .&. 0x07)
                 , chr' (ord0 + (w `unsafeShiftR` 3) .&. 0x07)
                 , chr' (ord0 + w .&. 0x07)
                 ]
       ord0 = 0x30 -- = ord '0'

-- | Emit a ".string" directive
pprString :: ByteString -> SDoc
pprString bs = text "\t.string " <> doubleQuotes (pprASCII bs)

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

pprSectionHeader :: NCGConfig -> Section -> SDoc
pprSectionHeader config (Section t suffix) =
 case platformOS (ncgPlatform config) of
   OSAIX     -> pprXcoffSectionHeader t
   OSDarwin  -> pprDarwinSectionHeader t
   OSMinGW32 -> pprGNUSectionHeader config (char '$') t suffix
   _         -> pprGNUSectionHeader config (char '.') t suffix

pprGNUSectionHeader :: NCGConfig -> SDoc -> SectionType -> CLabel -> SDoc
pprGNUSectionHeader config sep t suffix =
  text ".section " <> ptext header <> subsection <> flags
  where
    platform      = ncgPlatform config
    splitSections = ncgSplitSections config
    subsection
      | splitSections = sep <> ppr suffix
      | otherwise     = empty
    header = case t of
      Text -> sLit ".text"
      Data -> sLit ".data"
      ReadOnlyData  | OSMinGW32 <- platformOS platform
                                -> sLit ".rdata"
                    | otherwise -> sLit ".rodata"
      RelocatableReadOnlyData | OSMinGW32 <- platformOS platform
                                -- Concept does not exist on Windows,
                                -- So map these to R/O data.
                                          -> sLit ".rdata$rel.ro"
                              | otherwise -> sLit ".data.rel.ro"
      UninitialisedData -> sLit ".bss"
      ReadOnlyData16 | OSMinGW32 <- platformOS platform
                                 -> sLit ".rdata$cst16"
                     | otherwise -> sLit ".rodata.cst16"
      CString
        | OSMinGW32 <- platformOS platform
                    -> sLit ".rdata"
        | otherwise -> sLit ".rodata.str"
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
pprXcoffSectionHeader t = text $ case t of
     Text                    -> ".csect .text[PR]"
     Data                    -> ".csect .data[RW]"
     ReadOnlyData            -> ".csect .text[PR] # ReadOnlyData"
     RelocatableReadOnlyData -> ".csect .text[PR] # RelocatableReadOnlyData"
     ReadOnlyData16          -> ".csect .text[PR] # ReadOnlyData16"
     CString                 -> ".csect .text[PR] # CString"
     UninitialisedData       -> ".csect .data[BS]"
     OtherSection _          ->
       panic "PprBase.pprXcoffSectionHeader: unknown section type"

pprDarwinSectionHeader :: SectionType -> SDoc
pprDarwinSectionHeader t =
  ptext $ case t of
     Text -> sLit ".text"
     Data -> sLit ".data"
     ReadOnlyData -> sLit ".const"
     RelocatableReadOnlyData -> sLit ".const_data"
     UninitialisedData -> sLit ".data"
     ReadOnlyData16 -> sLit ".const"
     CString -> sLit ".section\t__TEXT,__cstring,cstring_literals"
     OtherSection _ ->
       panic "PprBase.pprDarwinSectionHeader: unknown section type"

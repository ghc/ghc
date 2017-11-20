-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module PprBase (
        castFloatToWord8Array,
        castDoubleToWord8Array,
        floatToBytes,
        doubleToBytes,
        pprASCII,
        pprSectionHeader
)

where

import GhcPrelude

import AsmUtils
import CLabel
import Cmm
import DynFlags
import FastString
import Outputable
import Platform

import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

import Control.Monad.ST

import Data.Word
import Data.Char



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

pprASCII :: [Word8] -> SDoc
pprASCII str
  -- Transform this given literal bytestring to escaped string and construct
  -- the literal SDoc directly.
  -- See Trac #14741
  -- and Note [Pretty print ASCII when AsmCodeGen]
  = text $ foldr (\w s -> (do1 . fromIntegral) w ++ s) "" str
    where
       do1 :: Int -> String
       do1 w | '\t' <- chr w = "\\t"
             | '\n' <- chr w = "\\n"
             | '"'  <- chr w = "\\\""
             | '\\' <- chr w = "\\\\"
             | isPrint (chr w) = [chr w]
             | otherwise = '\\' : octal w

       octal :: Int -> String
       octal w = [ chr (ord '0' + (w `div` 64) `mod` 8)
                 , chr (ord '0' + (w `div` 8) `mod` 8)
                 , chr (ord '0' + w `mod` 8)
                 ]

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
string in source code. See Trac #14741 for profiling results.
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

pprSectionHeader :: Platform -> Section -> SDoc
pprSectionHeader platform (Section t suffix) =
 case platformOS platform of
   OSAIX     -> pprXcoffSectionHeader t
   OSDarwin  -> pprDarwinSectionHeader t
   OSMinGW32 -> pprGNUSectionHeader (char '$') t suffix
   _         -> pprGNUSectionHeader (char '.') t suffix

pprGNUSectionHeader :: SDoc -> SectionType -> CLabel -> SDoc
pprGNUSectionHeader sep t suffix = sdocWithDynFlags $ \dflags ->
  let splitSections = gopt Opt_SplitSections dflags
      subsection | splitSections = sep <> ppr suffix
                 | otherwise     = empty
  in  text ".section " <> ptext (header dflags) <> subsection <>
      flags dflags
  where
    header dflags = case t of
      Text -> sLit ".text"
      Data -> sLit ".data"
      ReadOnlyData  | OSMinGW32 <- platformOS (targetPlatform dflags)
                                -> sLit ".rdata"
                    | otherwise -> sLit ".rodata"
      RelocatableReadOnlyData | OSMinGW32 <- platformOS (targetPlatform dflags)
                                -- Concept does not exist on Windows,
                                -- So map these to R/O data.
                                          -> sLit ".rdata$rel.ro"
                              | otherwise -> sLit ".data.rel.ro"
      UninitialisedData -> sLit ".bss"
      ReadOnlyData16 | OSMinGW32 <- platformOS (targetPlatform dflags)
                                 -> sLit ".rdata$cst16"
                     | otherwise -> sLit ".rodata.cst16"
      CString
        | OSMinGW32 <- platformOS (targetPlatform dflags)
                    -> sLit ".rdata"
        | otherwise -> sLit ".rodata.str"
      OtherSection _ ->
        panic "PprBase.pprGNUSectionHeader: unknown section type"
    flags dflags = case t of
      CString
        | OSMinGW32 <- platformOS (targetPlatform dflags)
                    -> empty
        | otherwise -> text ",\"aMS\"," <> sectionType "progbits" <> text ",1"
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

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
        pprSectionHeader,
        pprSectionHeaderData
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

import Foreign (withArrayLen)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Fingerprint (fingerprintData)
import GHC.Fingerprint.Type ()


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
pprSectionHeader platform sec = pprSectionHeaderData platform sec []

pprSectionHeaderData :: Platform -> Section -> [Word8] -> SDoc
pprSectionHeaderData platform (Section t suffix) datas =
 case platformOS platform of
   OSAIX     -> pprXcoffSectionHeader t
   OSDarwin  -> pprDarwinSectionHeader t
   OSMinGW32 -> pprGNUSectionHeader (char '$') t suffix datas
   _         -> pprGNUSectionHeader (char '.') t suffix datas

pprGNUSectionHeader :: SDoc -> SectionType -> CLabel -> [Word8] -> SDoc
pprGNUSectionHeader sep t suffix datas = sdocWithDynFlags $ \dflags ->
  let splitSections = gopt Opt_SplitSections dflags
      subsection = case (t, platformOS (targetPlatform dflags)) of
                     (CString, OSMinGW32) -> empty
                     _ | splitSections    -> sep <> ppr suffix
                       | otherwise        -> empty
  in  text ".section " <> header dflags <> subsection <>
      flags dflags
  where
    -- I'm not sure which optimization does this, but this optimization relies
    -- on constants being pooled.  This seems to be done at -O1 and higher
    -- but otherwise it is very unsafe to carry out.
    -- TODO: Check if the constant pooling is accidental or intended.
    use_pool dflags = optLevel dflags > 0

    fingerprintWord8 :: [Word8] -> String
    fingerprintWord8 word8s = show $ unsafeDupablePerformIO $
      withArrayLen word8s $ \len p ->
         fingerprintData p len

    header dflags = case t of
      Text -> text ".text"
      Data -> text ".data"
      ReadOnlyData  | OSMinGW32 <- platformOS (targetPlatform dflags)
                                -> text ".rdata"
                    | otherwise -> text ".rodata"
      RelocatableReadOnlyData | OSMinGW32 <- platformOS (targetPlatform dflags)
                                -- Concept does not exist on Windows,
                                -- but MinGW-w64 implements something similar
                                -- for .rdata_runtime_pseudo_reloc.
                                -- but this has a nasty bug
                                -- https://sourceforge.net/p/mingw-w64/bugs/537/
                                          -> text ".rdata"
                              | otherwise -> text ".data.rel.ro"
      UninitialisedData -> text ".bss"
      ReadOnlyData16 | OSMinGW32 <- platformOS (targetPlatform dflags)
                                 -> text ".rdata$cst16"
                     | otherwise -> text ".rodata.cst16"
      CString
        | OSMinGW32 <- platformOS (targetPlatform dflags)
            -> if use_pool dflags
                  then    text ".rdata$str." <> text (fingerprintWord8 datas)
                      $+$ text ".linkonce discard"
                  else text ".rdata$str"
        | otherwise -> text ".rodata.str"
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

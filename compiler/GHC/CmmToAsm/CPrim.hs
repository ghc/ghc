{-# LANGUAGE LambdaCase #-}

-- | Generating C symbol names emitted by the compiler.
module GHC.CmmToAsm.CPrim
    ( atomicReadLabel
    , atomicWriteLabel
    , atomicRMWLabel
    , cmpxchgLabel
    , xchgLabel
    , popCntLabel
    , pdepLabel
    , pextLabel
    , bSwapLabel
    , bRevLabel
    , clzLabel
    , ctzLabel
    , word2FloatLabel
    ) where

import GHC.Cmm.Type
import GHC.Cmm.MachOp
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic

popCntLabel :: Width -> FastString
popCntLabel = \case
  W8  -> fsLit "hs_popcnt8"
  W16 -> fsLit "hs_popcnt16"
  W32 -> fsLit "hs_popcnt32"
  W64 -> fsLit "hs_popcnt64"
  w   -> pprPanic "popCntLabel: Unsupported word width " (ppr w)

pdepLabel :: Width -> FastString
pdepLabel = \case
  W8  -> fsLit "hs_pdep8"
  W16 -> fsLit "hs_pdep16"
  W32 -> fsLit "hs_pdep32"
  W64 -> fsLit "hs_pdep64"
  w   -> pprPanic "pdepLabel: Unsupported word width " (ppr w)

pextLabel :: Width -> FastString
pextLabel = \case
  W8  -> fsLit "hs_pext8"
  W16 -> fsLit "hs_pext16"
  W32 -> fsLit "hs_pext32"
  W64 -> fsLit "hs_pext64"
  w   -> pprPanic "pextLabel: Unsupported word width " (ppr w)

bSwapLabel :: Width -> FastString
bSwapLabel = \case
  W16 -> fsLit "hs_bswap16"
  W32 -> fsLit "hs_bswap32"
  W64 -> fsLit "hs_bswap64"
  w   -> pprPanic "bSwapLabel: Unsupported word width " (ppr w)

bRevLabel :: Width -> FastString
bRevLabel = \case
  W8  -> fsLit "hs_bitrev8"
  W16 -> fsLit "hs_bitrev16"
  W32 -> fsLit "hs_bitrev32"
  W64 -> fsLit "hs_bitrev64"
  w   -> pprPanic "bRevLabel: Unsupported word width " (ppr w)

clzLabel :: Width -> FastString
clzLabel = \case
  W8  -> fsLit "hs_clz8"
  W16 -> fsLit "hs_clz16"
  W32 -> fsLit "hs_clz32"
  W64 -> fsLit "hs_clz64"
  w   -> pprPanic "clzLabel: Unsupported word width " (ppr w)

ctzLabel :: Width -> FastString
ctzLabel = \case
  W8  -> fsLit "hs_ctz8"
  W16 -> fsLit "hs_ctz16"
  W32 -> fsLit "hs_ctz32"
  W64 -> fsLit "hs_ctz64"
  w   -> pprPanic "ctzLabel: Unsupported word width " (ppr w)

word2FloatLabel :: Width -> FastString
word2FloatLabel = \case
  W32 -> fsLit "hs_word2float32"
  W64 -> fsLit "hs_word2float64"
  w   -> pprPanic "word2FloatLabel: Unsupported word width " (ppr w)

atomicRMWLabel :: Width -> AtomicMachOp -> FastString
atomicRMWLabel w amop = case amop of
  -- lots of boring cases, but we do it this way to get shared FastString
  -- literals (compared to concatenating strings and allocating FastStrings at
  -- runtime)
  AMO_Add  -> case w of
    W8  -> fsLit "hs_atomic_add8"
    W16 -> fsLit "hs_atomic_add16"
    W32 -> fsLit "hs_atomic_add32"
    W64 -> fsLit "hs_atomic_add64"
    _   -> pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)
  AMO_Sub  -> case w of
    W8  -> fsLit "hs_atomic_sub8"
    W16 -> fsLit "hs_atomic_sub16"
    W32 -> fsLit "hs_atomic_sub32"
    W64 -> fsLit "hs_atomic_sub64"
    _   -> pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)
  AMO_And  -> case w of
    W8  -> fsLit "hs_atomic_and8"
    W16 -> fsLit "hs_atomic_and16"
    W32 -> fsLit "hs_atomic_and32"
    W64 -> fsLit "hs_atomic_and64"
    _   -> pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)
  AMO_Nand  -> case w of
    W8  -> fsLit "hs_atomic_nand8"
    W16 -> fsLit "hs_atomic_nand16"
    W32 -> fsLit "hs_atomic_nand32"
    W64 -> fsLit "hs_atomic_nand64"
    _   -> pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)
  AMO_Or  -> case w of
    W8  -> fsLit "hs_atomic_or8"
    W16 -> fsLit "hs_atomic_or16"
    W32 -> fsLit "hs_atomic_or32"
    W64 -> fsLit "hs_atomic_or64"
    _   -> pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)
  AMO_Xor  -> case w of
    W8  -> fsLit "hs_atomic_xor8"
    W16 -> fsLit "hs_atomic_xor16"
    W32 -> fsLit "hs_atomic_xor32"
    W64 -> fsLit "hs_atomic_xor64"
    _   -> pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)


xchgLabel :: Width -> FastString
xchgLabel = \case
  W8  -> fsLit "hs_xchg8"
  W16 -> fsLit "hs_xchg16"
  W32 -> fsLit "hs_xchg32"
  W64 -> fsLit "hs_xchg64"
  w   -> pprPanic "xchgLabel: Unsupported word width " (ppr w)

cmpxchgLabel :: Width -> FastString
cmpxchgLabel = \case
  W8  -> fsLit "hs_cmpxchg8"
  W16 -> fsLit "hs_cmpxchg16"
  W32 -> fsLit "hs_cmpxchg32"
  W64 -> fsLit "hs_cmpxchg64"
  w   -> pprPanic "cmpxchgLabel: Unsupported word width " (ppr w)

atomicReadLabel :: Width -> FastString
atomicReadLabel = \case
  W8  -> fsLit "hs_atomicread8"
  W16 -> fsLit "hs_atomicread16"
  W32 -> fsLit "hs_atomicread32"
  W64 -> fsLit "hs_atomicread64"
  w   -> pprPanic "atomicReadLabel: Unsupported word width " (ppr w)

atomicWriteLabel :: Width -> FastString
atomicWriteLabel = \case
  W8  -> fsLit "hs_atomicwrite8"
  W16 -> fsLit "hs_atomicwrite16"
  W32 -> fsLit "hs_atomicwrite32"
  W64 -> fsLit "hs_atomicwrite64"
  w   -> pprPanic "atomicWriteLabel: Unsupported word width " (ppr w)

-- | Generating C symbol names emitted by the compiler.
module CPrim
    ( atomicReadLabel
    , atomicWriteLabel
    , atomicRMWLabel
    , cmpxchgLabel
    , popCntLabel
    , pdepLabel
    , pextLabel
    , bSwapLabel
    , clzLabel
    , ctzLabel
    , word2FloatLabel
    ) where

import GhcPrelude

import CmmType
import CmmMachOp
import Outputable

popCntLabel :: Width -> String
popCntLabel w = "hs_popcnt" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "popCntLabel: Unsupported word width " (ppr w)

pdepLabel :: Width -> String
pdepLabel w = "hs_pdep" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "pdepLabel: Unsupported word width " (ppr w)

pextLabel :: Width -> String
pextLabel w = "hs_pext" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "pextLabel: Unsupported word width " (ppr w)

bSwapLabel :: Width -> String
bSwapLabel w = "hs_bswap" ++ pprWidth w
  where
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "bSwapLabel: Unsupported word width " (ppr w)

clzLabel :: Width -> String
clzLabel w = "hs_clz" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "clzLabel: Unsupported word width " (ppr w)

ctzLabel :: Width -> String
ctzLabel w = "hs_ctz" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "ctzLabel: Unsupported word width " (ppr w)

word2FloatLabel :: Width -> String
word2FloatLabel w = "hs_word2float" ++ pprWidth w
  where
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "word2FloatLabel: Unsupported word width " (ppr w)

atomicRMWLabel :: Width -> AtomicMachOp -> String
atomicRMWLabel w amop = "hs_atomic_" ++ pprFunName amop ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "atomicRMWLabel: Unsupported word width " (ppr w)

    pprFunName AMO_Add  = "add"
    pprFunName AMO_Sub  = "sub"
    pprFunName AMO_And  = "and"
    pprFunName AMO_Nand = "nand"
    pprFunName AMO_Or   = "or"
    pprFunName AMO_Xor  = "xor"

cmpxchgLabel :: Width -> String
cmpxchgLabel w = "hs_cmpxchg" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "cmpxchgLabel: Unsupported word width " (ppr w)

atomicReadLabel :: Width -> String
atomicReadLabel w = "hs_atomicread" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "atomicReadLabel: Unsupported word width " (ppr w)

atomicWriteLabel :: Width -> String
atomicWriteLabel w = "hs_atomicwrite" ++ pprWidth w
  where
    pprWidth W8  = "8"
    pprWidth W16 = "16"
    pprWidth W32 = "32"
    pprWidth W64 = "64"
    pprWidth w   = pprPanic "atomicWriteLabel: Unsupported word width " (ppr w)

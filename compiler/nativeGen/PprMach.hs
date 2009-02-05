{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

-- We start with the @pprXXX@s with some cross-platform commonality
-- (e.g., 'pprReg'); we conclude with the no-commonality monster,
-- 'pprInstr'.

#include "nativeGen/NCG.h"

module PprMach ( 
	pprNatCmmTop, pprBasicBlock, pprSectionHeader, pprData,
	pprInstr, pprSize, pprUserReg, pprImm
  ) where

#include "HsVersions.h"

import PprBase

import BlockId
import Cmm
import Regs		-- may differ per-platform
import Instrs
import Regs

import CLabel		( CLabel, pprCLabel, externallyVisibleCLabel,
			  labelDynamic, mkAsmTempLabel, entryLblToInfoLbl )
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
import CLabel       ( mkDeadStripPreventer )
#endif

import Panic		( panic )
import Unique		( pprUnique )
import Pretty
import FastString
import qualified Outputable
import Outputable	( Outputable, pprPanic, ppr, docToSDoc)

import Data.Array.ST
import Data.Word	( Word8 )
import Control.Monad.ST
import Data.Char	( chr, ord )
import Data.Maybe       ( isJust )


#if   alpha_TARGET_ARCH
import Alpha.Ppr
#elif powerpc_TARGET_ARCH
import PPC.Ppr
#elif i386_TARGET_ARCH || x86_64_TARGET_ARCH
import X86.Ppr
#elif sparc_TARGET_ARCH
import SPARC.Ppr
#else
#error "Regs: not defined for this architecture"
#endif



-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmTop :: NatCmmTop -> Doc
pprNatCmmTop (CmmData section dats) = 
  pprSectionHeader section $$ vcat (map pprData dats)

 -- special case for split markers:
pprNatCmmTop (CmmProc [] lbl _ (ListGraph [])) = pprLabel lbl

pprNatCmmTop (CmmProc info lbl params (ListGraph blocks)) = 
  pprSectionHeader Text $$
  (if null info then -- blocks guaranteed not null, so label needed
       pprLabel lbl
   else
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
            pprCLabel_asm (mkDeadStripPreventer $ entryLblToInfoLbl lbl)
                <> char ':' $$
#endif
       vcat (map pprData info) $$
       pprLabel (entryLblToInfoLbl lbl)
  ) $$
  vcat (map pprBasicBlock blocks)
     -- above: Even the first block gets a label, because with branch-chain
     -- elimination, it might be the target of a goto.
#if HAVE_SUBSECTIONS_VIA_SYMBOLS
        -- If we are using the .subsections_via_symbols directive
        -- (available on recent versions of Darwin),
        -- we have to make sure that there is some kind of reference
        -- from the entry code to a label on the _top_ of of the info table,
        -- so that the linker will not think it is unreferenced and dead-strip
        -- it. That's why the label is called a DeadStripPreventer (_dsp).
  $$ if not (null info)
		    then text "\t.long "
		      <+> pprCLabel_asm (entryLblToInfoLbl lbl)
		      <+> char '-'
		      <+> pprCLabel_asm (mkDeadStripPreventer $ entryLblToInfoLbl lbl)
		    else empty
#endif


pprBasicBlock :: NatBasicBlock -> Doc
pprBasicBlock (BasicBlock (BlockId id) instrs) =
  pprLabel (mkAsmTempLabel id) $$
  vcat (map pprInstr instrs)


pprData :: CmmStatic -> Doc
pprData (CmmAlign bytes)         = pprAlign bytes
pprData (CmmDataLabel lbl)       = pprLabel lbl
pprData (CmmString str)          = pprASCII str
pprData (CmmUninitialised bytes) = ptext (sLit s) <> int bytes
    where s =
#if defined(solaris2_TARGET_OS)
              ".skip "
#else
              ".space "
#endif
pprData (CmmStaticLit lit)       = pprDataItem lit

pprGloblDecl :: CLabel -> Doc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext IF_ARCH_sparc((sLit ".global "), 
				    (sLit ".globl ")) <>
		pprCLabel_asm lbl

pprTypeAndSizeDecl :: CLabel -> Doc
pprTypeAndSizeDecl lbl
#if linux_TARGET_OS
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".type ") <>
		pprCLabel_asm lbl <> ptext (sLit ", @object")
#else
  = empty
#endif

pprLabel :: CLabel -> Doc
pprLabel lbl = pprGloblDecl lbl $$ pprTypeAndSizeDecl lbl $$ (pprCLabel_asm lbl <> char ':')


pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> Doc
       do1 w = ptext (sLit "\t.byte\t") <> int (fromIntegral w)

pprAlign bytes =
	IF_ARCH_alpha(ptext (sLit ".align ") <> int pow2,
	IF_ARCH_i386(ptext (sLit ".align ") <> int IF_OS_darwin(pow2,bytes),
	IF_ARCH_x86_64(ptext (sLit ".align ") <> int IF_OS_darwin(pow2,bytes),
	IF_ARCH_sparc(ptext (sLit ".align ") <> int bytes,
	IF_ARCH_powerpc(ptext (sLit ".align ") <> int pow2,)))))
  where
	pow2 = log2 bytes
	
	log2 :: Int -> Int  -- cache the common ones
	log2 1 = 0 
	log2 2 = 1
	log2 4 = 2
	log2 8 = 3
	log2 n = 1 + log2 (n `quot` 2)


-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr	 instr	= Outputable.docToSDoc $ pprInstr instr





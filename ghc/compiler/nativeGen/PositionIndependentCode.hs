#include "../includes/ghcconfig.h"

module PositionIndependentCode (
        cmmMakeDynamicReference,
        needImportedSymbols,
        pprImportedSymbol,
        pprGotDeclaration,
        initializePicBase
     ) where

{-
  This module handles generation of position independent code and
  dynamic-linking related issues for the native code generator.
  
  Things outside this module which are related to this:
  
  + module CLabel
    - PIC base label (pretty printed as local label 1)
    - DynamicLinkerLabels - several kinds:
        CodeStub, SymbolPtr, GotSymbolPtr, GotSymbolOffset
    - labelDynamic predicate
  + module Cmm
    - The CmmExpr datatype has a CmmPicBaseReg constructor
    - The CmmLit datatype has a CmmLabelDiffOff constructor
  + codeGen & RTS
    - When tablesNextToCode, no absolute addresses are stored in info tables
      any more. Instead, offsets from the info label are used.
    - For Win32 only, SRTs might contain addresses of __imp_ symbol pointers
      because Win32 doesn't support external references in data sections.
      TODO: make sure this still works, it might be bitrotted
  + NCG
    - The cmmToCmm pass in AsmCodeGen calls cmmMakeDynamicReference for all
      labels.
    - nativeCodeGen calls pprImportedSymbol and pprGotDeclaration to output
      all the necessary stuff for imported symbols.
    - The NCG monad keeps track of a list of imported symbols.
    - MachCodeGen invokes initializePicBase to generate code to initialize
      the PIC base register when needed.
    - MachCodeGen calls cmmMakeDynamicReference whenever it uses a CLabel
      that wasn't in the original Cmm code (e.g. floating point literals).
  + The Mangler
    - The mangler converts absolure refs to relative refs in info tables
    - Symbol pointers, stub code and PIC calculations that are generated
      by GCC are left intact by the mangler (so far only on ppc-darwin
      and ppc-linux).
-}
     
#include "HsVersions.h"
#include "nativeGen/NCG.h"

import Cmm
import MachOp           ( MachOp(MO_Add), wordRep )
import CLabel           ( CLabel, pprCLabel,
                          mkDynamicLinkerLabel, DynamicLinkerLabelInfo(..),
                          dynamicLinkerLabelInfo, mkPicBaseLabel,
                          labelDynamic, externallyVisibleCLabel )

import MachRegs
import MachInstrs
import NCGMonad         ( NatM, getNewRegNat, getNewLabelNat )

import CmdLineOpts      ( opt_PIC )

import Pretty
import qualified Outputable

import Panic            ( panic )


-- The most important function here is cmmMakeDynamicReference.

-- It gets called by the cmmToCmm pass for every CmmLabel in the Cmm
-- code. It does The Right Thing(tm) to convert the CmmLabel into a
-- position-independent, dynamic-linking-aware reference to the thing
-- in question.
-- Note that this also has to be called from MachCodeGen in order to
-- access static data like floating point literals (labels that were
-- created after the cmmToCmm pass).
-- The function must run in a monad that can keep track of imported symbols
-- A function for recording an imported symbol must be passed in:
-- - addImportCmmOpt for the CmmOptM monad
-- - addImportNat for the NatM monad.

cmmMakeDynamicReference
  :: Monad m => (CLabel -> m ())  -- a monad & a function
                                  -- used for recording imported symbols
             -> Bool              -- whether this is the target of a jump
             -> CLabel            -- the label
             -> m CmmExpr
  
cmmMakeDynamicReference addImport isJumpTarget lbl
  | Just _ <- dynamicLinkerLabelInfo lbl
  = return $ CmmLit $ CmmLabel lbl   -- already processed it, pass through
  | otherwise = case howToAccessLabel isJumpTarget lbl of
        AccessViaStub -> do
              let stub = mkDynamicLinkerLabel CodeStub lbl
              addImport stub
              return $ CmmLit $ CmmLabel stub
        AccessViaSymbolPtr -> do
              let symbolPtr = mkDynamicLinkerLabel SymbolPtr lbl
              addImport symbolPtr
              return $ CmmLoad (cmmMakePicReference symbolPtr) wordRep
        AccessDirectly
                -- all currently supported processors support
                -- a PC-relative branch instruction, so just jump there
          | isJumpTarget -> return $ CmmLit $ CmmLabel lbl
                -- for data, we might have to make some calculations:
          | otherwise    -> return $ cmmMakePicReference lbl  
  
-- -------------------------------------------------------------------
  
-- Create a position independent reference to a label.
-- (but do not bother with dynamic linking).
-- We calculate the label's address by adding some (platform-dependent)
-- offset to our base register; this offset is calculated by
-- the function picRelative in the platform-dependent part below.

cmmMakePicReference :: CLabel -> CmmExpr
  
#if !mingw32_TARGET_OS
        -- Windows doesn't need PIC,
        -- everything gets relocated at runtime

cmmMakePicReference lbl
    | opt_PIC && absoluteLabel lbl = CmmMachOp (MO_Add wordRep) [
            CmmPicBaseReg,
            CmmLit $ picRelative lbl
        ]
    where
        absoluteLabel lbl = case dynamicLinkerLabelInfo lbl of
                                Just (GotSymbolPtr, _) -> False
                                Just (GotSymbolOffset, _) -> False
                                _ -> True

#endif
cmmMakePicReference lbl = CmmLit $ CmmLabel lbl

-- ===================================================================
-- Platform dependent stuff
-- ===================================================================

-- Knowledge about how special dynamic linker labels like symbol
-- pointers, code stubs and GOT offsets look like is located in the
-- module CLabel.

-- -------------------------------------------------------------------

-- We have to decide which labels need to be accessed
-- indirectly or via a piece of stub code.

data LabelAccessStyle = AccessViaStub
                      | AccessViaSymbolPtr
                      | AccessDirectly

howToAccessLabel :: Bool -> CLabel -> LabelAccessStyle

#if mingw32_TARGET_OS
-- Windows
-- 
-- We need to use access *exactly* those things that
-- are imported from a DLL via an __imp_* label.
-- There are no stubs for imported code.

howToAccessLabel _ lbl | labelDynamic lbl = AccessViaSymbolPtr
                       | otherwise        = AccessDirectly

#elif darwin_TARGET_OS
-- Mach-O (Darwin, Mac OS X)
--
-- Indirect access is required in the following cases:
-- * things imported from a dynamic library
-- * things from a different module, if we're generating PIC code
-- It is always possible to access something indirectly,
-- even when it's not necessary.

howToAccessLabel True lbl
      -- jumps to a dynamic library go via a symbol stub
    | labelDynamic lbl = AccessViaStub
      -- when generating PIC code, all cross-module references must
      -- must go via a symbol pointer, too.
      -- Unfortunately, we don't know whether it's cross-module,
      -- so we do it for all externally visible labels.
      -- This is a slight waste of time and space, but otherwise
      -- we'd need to pass the current Module all the way in to
      -- this function.
    | opt_PIC && externallyVisibleCLabel lbl = AccessViaStub
howToAccessLabel False lbl
      -- data access to a dynamic library goes via a symbol pointer
    | labelDynamic lbl = AccessViaSymbolPtr
      -- cross-module PIC references: same as above
    | opt_PIC && externallyVisibleCLabel lbl = AccessViaSymbolPtr
howToAccessLabel _ _ = AccessDirectly

#elif linux_TARGET_OS && powerpc_TARGET_ARCH
-- PowerPC Linux
--
-- PowerPC Linux is just plain broken.
-- While it's theoretically possible to use GOT offsets larger
-- than 16 bit, the standard crt*.o files don't, which leads to
-- linker errors as soon as the GOT size exceeds 16 bit.
-- Also, the assembler doesn't support @gotoff labels.
-- In order to be able to use a larger GOT, we circumvent the
-- entire GOT mechanism and do it ourselves (this is what GCC does).

-- In this scheme, we need to do _all data references_ (even refs
-- to static data) via a SymbolPtr when we are generating PIC.
-- Luckily, the PLT works as expected, so we can simply access
-- dynamically linked code via the PLT.

howToAccessLabel _ _ | not opt_PIC = AccessDirectly
howToAccessLabel True lbl
    = if labelDynamic lbl then AccessViaStub
                          else AccessDirectly
howToAccessLabel False lbl
    = AccessViaSymbolPtr

#elif linux_TARGET_OS
-- ELF (Linux)
--
-- Indirect access is required for references to imported symbols
-- from position independent code.
-- It is always possible to access something indirectly,
-- even when it's not necessary.

-- For code, we can use a relative jump to a piece of
-- stub code instead (this allows lazy binding of imported symbols).

howToAccessLabel isJump lbl
        -- no PIC -> the dynamic linker does everything for us
   | not opt_PIC = AccessDirectly
        -- if it's not imported, we need no indirection
        -- ("foo" will end up being accessed as "foo@GOTOFF")
   | not (labelDynamic lbl) = AccessDirectly
#if !i386_TARGET_ARCH
-- for Intel, we temporarily disable the use of the
-- Procedure Linkage Table, because PLTs on intel require the
-- address of the GOT to be loaded into register %ebx before
-- a jump through the PLT is made.
-- TODO: make the i386 NCG ensure this before jumping to a
--       CodeStub label, so we can remove this special case.
   | isJump = AccessViaStub
#endif
   | otherwise = AccessViaSymbolPtr

#else
howToAccessLabel = panic "PositionIndependentCode.howToAccessLabel"
#endif

-- -------------------------------------------------------------------

-- What do we have to add to our 'PIC base register' in order to
-- get the address of a label?

picRelative :: CLabel -> CmmLit
#if darwin_TARGET_OS
-- Darwin:
-- The PIC base register points to the PIC base label at the beginning
-- of the current CmmTop. We just have to use a label difference to
-- get the offset.
-- We have already made sure that all labels that are not from the current
-- module are accessed indirectly ('as' can't calculate differences between
-- undefined labels).

picRelative lbl
  = CmmLabelDiffOff lbl mkPicBaseLabel 0

#elif powerpc_TARGET_ARCH && linux_TARGET_OS
-- PowerPC Linux:
-- The PIC base register points to our fake GOT. Use a label difference
-- to get the offset.
-- We have made sure that *everything* is accessed indirectly, so this
-- is only used for offsets from the GOT to symbol pointers inside the
-- GOT.
picRelative lbl
  = CmmLabelDiffOff lbl gotLabel 0

#elif linux_TARGET_OS
-- Other Linux versions:
-- The PIC base register points to the GOT. Use foo@got for symbol
-- pointers, and foo@gotoff for everything else.

picRelative lbl
  | Just (SymbolPtr, lbl') <- dynamicLinkerLabelInfo lbl
  = CmmLabel $ mkDynamicLinkerLabel GotSymbolPtr lbl'
  | otherwise
  = CmmLabel $ mkDynamicLinkerLabel GotSymbolOffset lbl

#else
picRelative lbl = panic "PositionIndependentCode.picRelative"
#endif

-- -------------------------------------------------------------------

-- What do we have to add to every assembly file we generate?

-- utility function for pretty-printing asm-labels,
-- copied from PprMach
asmSDoc d = Outputable.withPprStyleDoc (
	      Outputable.mkCodeStyle Outputable.AsmStyle) d
pprCLabel_asm l = asmSDoc (pprCLabel l)


#if darwin_TARGET_OS

needImportedSymbols = True

-- We don't need to declare any offset tables
pprGotDeclaration = Pretty.empty

-- On Darwin, we have to generate our own stub code for lazy binding..
-- There are two versions, one for PIC and one for non-PIC.
pprImportedSymbol importedLbl
    | Just (CodeStub, lbl) <- dynamicLinkerLabelInfo importedLbl
    = case opt_PIC of
        False ->
            vcat [
                ptext SLIT(".symbol_stub"),
                ptext SLIT("L") <> pprCLabel_asm lbl <> ptext SLIT("$stub:"),
                    ptext SLIT("\t.indirect_symbol") <+> pprCLabel_asm lbl,
                    ptext SLIT("\tlis r11,ha16(L") <> pprCLabel_asm lbl
                        <> ptext SLIT("$lazy_ptr)"),
                    ptext SLIT("\tlwz r12,lo16(L") <> pprCLabel_asm lbl
                        <> ptext SLIT("$lazy_ptr)(r11)"),
                    ptext SLIT("\tmtctr r12"),
                    ptext SLIT("\taddi r11,r11,lo16(L") <> pprCLabel_asm lbl
                        <> ptext SLIT("$lazy_ptr)"),
                    ptext SLIT("\tbctr")
            ]
        True ->
            vcat [
                ptext SLIT(".section __TEXT,__picsymbolstub1,")
                  <> ptext SLIT("symbol_stubs,pure_instructions,32"),
                ptext SLIT("L") <> pprCLabel_asm lbl <> ptext SLIT("$stub:"),
                    ptext SLIT("\t.indirect_symbol") <+> pprCLabel_asm lbl,
                    ptext SLIT("\tmflr r0"),
                    ptext SLIT("\tbcl 20,31,L0$") <> pprCLabel_asm lbl,
                ptext SLIT("L0$") <> pprCLabel_asm lbl <> char ':',
                    ptext SLIT("\tmflr r11"),
                    ptext SLIT("\taddis r11,r11,ha16(L") <> pprCLabel_asm lbl
                        <> ptext SLIT("$lazy_ptr-L0$") <> pprCLabel_asm lbl <> char ')',
                    ptext SLIT("\tmtlr r0"),
                    ptext SLIT("\tlwzu r12,lo16(L") <> pprCLabel_asm lbl
                        <> ptext SLIT("$lazy_ptr-L0$") <> pprCLabel_asm lbl
                        <> ptext SLIT(")(r11)"),
                    ptext SLIT("\tmtctr r12"),
                    ptext SLIT("\tbctr")
            ]
    $+$ vcat [
        ptext SLIT(".lazy_symbol_pointer"),
        ptext SLIT("L") <> pprCLabel_asm lbl <> ptext SLIT("$lazy_ptr:"),
            ptext SLIT("\t.indirect_symbol") <+> pprCLabel_asm lbl,
            ptext SLIT("\t.long dyld_stub_binding_helper")
    ]

-- We also have to declare our symbol pointers ourselves:
    | Just (SymbolPtr, lbl) <- dynamicLinkerLabelInfo importedLbl
    = vcat [
        ptext SLIT(".non_lazy_symbol_pointer"),
        char 'L' <> pprCLabel_asm lbl <> ptext SLIT("$non_lazy_ptr:"),
	    ptext SLIT("\t.indirect_symbol") <+> pprCLabel_asm lbl,
            ptext SLIT("\t.long\t0")
    ]

    | otherwise = empty

#elif powerpc_TARGET_ARCH && linux_TARGET_OS

-- For PowerPC linux, we don't do anything unless we're generating PIC.
needImportedSymbols = opt_PIC

-- If we're generating PIC, we need to create our own "fake GOT".

gotLabel = mkForeignLabel -- HACK: it's not really foreign
                           FSLIT(".LCTOC1") Nothing False

-- The .LCTOC1 label is defined to point 32768 bytes into the table,
-- to make the most of the PPC's 16-bit displacements.

pprGotDeclaration = vcat [
        ptext SLIT(".section \".got2\",\"aw\""),
        ptext SLIT(".LCTOC1 = .+32768")
    ]

-- We generate one .long literal for every symbol we import;
-- the dynamic linker will relocate those addresses.
    
pprImportedSymbol importedLbl
    | Just (SymbolPtr, lbl) <- dynamicLinkerLabelInfo importedLbl
    vcat [
        ptext SLIT(".section \".got2\", \"aw\""),
        ptext SLIT(".LC_") <> pprCLabel_asm lbl <> char ':',
        ptext SLIT("\t.long") <+> pprCLabel_asm lbl
    ]

-- PLT code stubs are generated automatically be the dynamic linker.
    | otherwise = empty

#else

-- For all other currently supported platforms, we don't need to do
-- anything at all.

needImportedSymbols = False
pprGotDeclaration = Pretty.empty
pprImportedSymbol _ = empty
#endif

-- -------------------------------------------------------------------

-- Generate code to calculate the address that should be put in the
-- PIC base register.
-- This is called by MachCodeGen for every CmmProc that accessed the
-- PIC base register. It adds the appropriate instructions to the
-- top of the CmmProc.

-- It is assumed that the first NatCmmTop in the input list is a Proc
-- and the rest are CmmDatas.

initializePicBase :: Reg -> [NatCmmTop] -> NatM [NatCmmTop]

#if powerpc_TARGET_ARCH && darwin_TARGET_OS

-- Darwin is simple: just fetch the address of a local label.
initializePicBase picReg (CmmProc info lab params blocks : statics)
    = return (CmmProc info lab params (b':tail blocks) : statics)
    where BasicBlock bID insns = head blocks
          b' = BasicBlock bID (FETCHPC picReg : insns)

#elif powerpc_TARGET_ARCH && linux_TARGET_OS

-- Get a pointer to our own fake GOT, which is defined on a per-module basis.
-- This is exactly how GCC does it, and it's quite horrible:
-- We first fetch the address of a local label (mkPicBaseLabel).
-- Then we add a 16-bit offset to that to get the address of a .long that we
-- define in .text space right next to the proc. This .long literal contains
-- the (32-bit) offset from our local label to our global offset table
-- (.LCTOC1 aka gotOffLabel).
initializePicBase picReg
    (CmmProc info lab params blocks : statics)
    = do
        gotOffLabel <- getNewLabelNat
        tmp <- getNewRegNat wordRep
        let 
            gotOffset = CmmData Text [
                            CmmDataLabel gotOffLabel,
			    CmmStaticLit (CmmLabelDiffOff gotLabel
                        	                          mkPicBaseLabel
				                          0)
                        ]
            offsetToOffset = ImmConstantDiff (ImmCLbl gotOffLabel)
                                             (ImmCLbl mkPicBaseLabel)
            BasicBlock bID insns = head blocks
            b' = BasicBlock bID (FETCHPC picReg
                               : LD wordRep tmp
                                    (AddrRegImm picReg offsetToOffset)
                               : ADD picReg picReg (RIReg tmp)
                               : insns)
        return (CmmProc info lab params (b' : tail blocks) : gotOffset : statics)
#else
initializePicBase picReg proc = panic "initializePicBase"

-- TODO:
-- i386_TARGET_ARCH && linux_TARGET_OS:
-- generate something like:
--              call 1f
-- 1:           popl %picReg
--              addl __GLOBAL_OFFSET_TABLE__+.-1b, %picReg
-- It might be a good idea to use a FETCHPC pseudo-instruction (like for PowerPC)
-- in order to avoid having to create a new basic block.
-- ((FETCHPC reg) should pretty-print as call 1f; 1: popl reg)

-- mingw32_TARGET_OS: not needed, won't be called

-- i386_TARGET_ARCH && darwin_TARGET_OS:
-- (just for completeness ;-)
--              call 1f
-- 1:           popl %picReg
#endif

{-
  This module handles generation of position independent code and
  dynamic-linking related issues for the native code generator.

  This depends both the architecture and OS, so we define it here
  instead of in one of the architecture specific modules.

  Things outside this module which are related to this:

  + module CLabel
    - PIC base label (pretty printed as local label 1)
    - DynamicLinkerLabels - several kinds:
        CodeStub, SymbolPtr, GotSymbolPtr, GotSymbolOffset
    - labelDynamic predicate
  + module Cmm
    - The GlobalReg datatype has a PicBaseReg constructor
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
-}

module PIC (
        cmmMakeDynamicReference,
        ReferenceKind(..),
        needImportedSymbols,
        pprImportedSymbol,
        pprGotDeclaration,

        initializePicBase_ppc,
        initializePicBase_x86
)

where

import qualified PPC.Instr      as PPC
import qualified PPC.Regs       as PPC

import qualified X86.Instr      as X86

import Platform
import Instruction
import Size
import Reg
import NCGMonad


import Hoopl
import Cmm
import CLabel           ( CLabel, ForeignLabelSource(..), pprCLabel,
                          mkDynamicLinkerLabel, DynamicLinkerLabelInfo(..),
                          dynamicLinkerLabelInfo, mkPicBaseLabel,
                          labelDynamic, externallyVisibleCLabel )

import CLabel           ( mkForeignLabel )


import BasicTypes

import Outputable

import DynFlags
import FastString



--------------------------------------------------------------------------------
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

data ReferenceKind
        = DataReference
        | CallReference
        | JumpReference
        deriving(Eq)


cmmMakeDynamicReference, cmmMakeDynamicReference'
  :: Monad m => DynFlags
             -> (CLabel -> m ())  -- a monad & a function
                                  -- used for recording imported symbols
             -> ReferenceKind     -- whether this is the target of a jump
             -> CLabel            -- the label
             -> m CmmExpr

cmmMakeDynamicReference = cmmMakeDynamicReference'

cmmMakeDynamicReference' dflags addImport referenceKind lbl
  | Just _ <- dynamicLinkerLabelInfo lbl
  = return $ CmmLit $ CmmLabel lbl   -- already processed it, pass through

  | otherwise
  = case howToAccessLabel
                dflags
                (platformArch $ targetPlatform dflags)
                (platformOS   $ targetPlatform dflags)
                referenceKind lbl of

        AccessViaStub -> do
              let stub = mkDynamicLinkerLabel CodeStub lbl
              addImport stub
              return $ CmmLit $ CmmLabel stub

        AccessViaSymbolPtr -> do
              let symbolPtr = mkDynamicLinkerLabel SymbolPtr lbl
              addImport symbolPtr
              return $ CmmLoad (cmmMakePicReference dflags symbolPtr) (bWord dflags)

        AccessDirectly -> case referenceKind of
                -- for data, we might have to make some calculations:
              DataReference -> return $ cmmMakePicReference dflags lbl
                -- all currently supported processors support
                -- PC-relative branch and call instructions,
                -- so just jump there if it's a call or a jump
              _ -> return $ CmmLit $ CmmLabel lbl


-- -----------------------------------------------------------------------------
-- Create a position independent reference to a label.
-- (but do not bother with dynamic linking).
-- We calculate the label's address by adding some (platform-dependent)
-- offset to our base register; this offset is calculated by
-- the function picRelative in the platform-dependent part below.

cmmMakePicReference :: DynFlags -> CLabel -> CmmExpr
cmmMakePicReference dflags lbl

        -- Windows doesn't need PIC,
        -- everything gets relocated at runtime
        | OSMinGW32 <- platformOS $ targetPlatform dflags
        = CmmLit $ CmmLabel lbl


        | (gopt Opt_PIC dflags || not (gopt Opt_Static dflags)) && absoluteLabel lbl
        = CmmMachOp (MO_Add (wordWidth dflags))
                [ CmmReg (CmmGlobal PicBaseReg)
                , CmmLit $ picRelative
                                (platformArch   $ targetPlatform dflags)
                                (platformOS     $ targetPlatform dflags)
                                lbl ]

        | otherwise
        = CmmLit $ CmmLabel lbl


absoluteLabel :: CLabel -> Bool
absoluteLabel lbl
 = case dynamicLinkerLabelInfo lbl of
        Just (GotSymbolPtr, _)    -> False
        Just (GotSymbolOffset, _) -> False
        _                         -> True


--------------------------------------------------------------------------------
-- Knowledge about how special dynamic linker labels like symbol
-- pointers, code stubs and GOT offsets look like is located in the
-- module CLabel.

-- We have to decide which labels need to be accessed
-- indirectly or via a piece of stub code.
data LabelAccessStyle
        = AccessViaStub
        | AccessViaSymbolPtr
        | AccessDirectly

howToAccessLabel
        :: DynFlags -> Arch -> OS -> ReferenceKind -> CLabel -> LabelAccessStyle


-- Windows
-- In Windows speak, a "module" is a set of objects linked into the
-- same Portable Exectuable (PE) file. (both .exe and .dll files are PEs).
--
-- If we're compiling a multi-module program then symbols from other modules
-- are accessed by a symbol pointer named __imp_SYMBOL. At runtime we have the
-- following.
--
--   (in the local module)
--     __imp_SYMBOL: addr of SYMBOL
--
--   (in the other module)
--     SYMBOL: the real function / data.
--
-- To access the function at SYMBOL from our local module, we just need to
-- dereference the local __imp_SYMBOL.
--
-- If Opt_Static is set then we assume that all our code will be linked
-- into the same .exe file. In this case we always access symbols directly,
-- and never use __imp_SYMBOL.
--
howToAccessLabel dflags _ OSMinGW32 _ lbl

        -- Assume all symbols will be in the same PE, so just access them directly.
        | gopt Opt_Static dflags
        = AccessDirectly

        -- If the target symbol is in another PE we need to access it via the
        --      appropriate __imp_SYMBOL pointer.
        | labelDynamic dflags (thisPackage dflags) lbl
        = AccessViaSymbolPtr

        -- Target symbol is in the same PE as the caller, so just access it directly.
        | otherwise
        = AccessDirectly


-- Mach-O (Darwin, Mac OS X)
--
-- Indirect access is required in the following cases:
--  * things imported from a dynamic library
--  * (not on x86_64) data from a different module, if we're generating PIC code
-- It is always possible to access something indirectly,
-- even when it's not necessary.
--
howToAccessLabel dflags arch OSDarwin DataReference lbl
        -- data access to a dynamic library goes via a symbol pointer
        | labelDynamic dflags (thisPackage dflags) lbl
        = AccessViaSymbolPtr

        -- when generating PIC code, all cross-module data references must
        -- must go via a symbol pointer, too, because the assembler
        -- cannot generate code for a label difference where one
        -- label is undefined. Doesn't apply t x86_64.
        -- Unfortunately, we don't know whether it's cross-module,
        -- so we do it for all externally visible labels.
        -- This is a slight waste of time and space, but otherwise
        -- we'd need to pass the current Module all the way in to
        -- this function.
        | arch /= ArchX86_64
        , gopt Opt_PIC dflags && externallyVisibleCLabel lbl
        = AccessViaSymbolPtr

        | otherwise
        = AccessDirectly

howToAccessLabel dflags arch OSDarwin JumpReference lbl
        -- dyld code stubs don't work for tailcalls because the
        -- stack alignment is only right for regular calls.
        -- Therefore, we have to go via a symbol pointer:
        | arch == ArchX86 || arch == ArchX86_64
        , labelDynamic dflags (thisPackage dflags) lbl
        = AccessViaSymbolPtr


howToAccessLabel dflags arch OSDarwin _ lbl
        -- Code stubs are the usual method of choice for imported code;
        -- not needed on x86_64 because Apple's new linker, ld64, generates
        -- them automatically.
        | arch /= ArchX86_64
        , labelDynamic dflags (thisPackage dflags) lbl
        = AccessViaStub

        | otherwise
        = AccessDirectly

-- ELF (Linux)
--
-- ELF tries to pretend to the main application code that dynamic linking does
-- not exist. While this may sound convenient, it tends to mess things up in
-- very bad ways, so we have to be careful when we generate code for the main
-- program (-dynamic but no -fPIC).
--
-- Indirect access is required for references to imported symbols
-- from position independent code. It is also required from the main program
-- when dynamic libraries containing Haskell code are used.

howToAccessLabel _ ArchPPC_64 os kind _
        | osElfTarget os
        = if kind == DataReference
            -- ELF PPC64 (powerpc64-linux), AIX, MacOS 9, BeOS/PPC
            then AccessViaSymbolPtr
            -- actually, .label instead of label
            else AccessDirectly

howToAccessLabel dflags _ os _ _
        -- no PIC -> the dynamic linker does everything for us;
        --           if we don't dynamically link to Haskell code,
        --           it actually manages to do so without messing thins up.
        | osElfTarget os
        , not (gopt Opt_PIC dflags) && gopt Opt_Static dflags
        = AccessDirectly

howToAccessLabel dflags arch os DataReference lbl
        | osElfTarget os
        = case () of
            -- A dynamic label needs to be accessed via a symbol pointer.
          _ | labelDynamic dflags (thisPackage dflags) lbl
            -> AccessViaSymbolPtr

            -- For PowerPC32 -fPIC, we have to access even static data
            -- via a symbol pointer (see below for an explanation why
            -- PowerPC32 Linux is especially broken).
            | arch == ArchPPC
            , gopt Opt_PIC dflags
            -> AccessViaSymbolPtr

            | otherwise
            -> AccessDirectly


        -- In most cases, we have to avoid symbol stubs on ELF, for the following reasons:
        --   on i386, the position-independent symbol stubs in the Procedure Linkage Table
        --   require the address of the GOT to be loaded into register %ebx on entry.
        --   The linker will take any reference to the symbol stub as a hint that
        --   the label in question is a code label. When linking executables, this
        --   will cause the linker to replace even data references to the label with
        --   references to the symbol stub.

        -- This leaves calling a (foreign) function from non-PIC code
        -- (AccessDirectly, because we get an implicit symbol stub)
        -- and calling functions from PIC code on non-i386 platforms (via a symbol stub)

howToAccessLabel dflags arch os CallReference lbl
        | osElfTarget os
        , labelDynamic dflags (thisPackage dflags) lbl && not (gopt Opt_PIC dflags)
        = AccessDirectly

        | osElfTarget os
        , arch /= ArchX86
        , labelDynamic dflags (thisPackage dflags) lbl && gopt Opt_PIC dflags
        = AccessViaStub

howToAccessLabel dflags _ os _ lbl
        | osElfTarget os
        = if labelDynamic dflags (thisPackage dflags) lbl
            then AccessViaSymbolPtr
            else AccessDirectly

-- all other platforms
howToAccessLabel dflags _ _ _ _
        | not (gopt Opt_PIC dflags)
        = AccessDirectly

        | otherwise
        = panic "howToAccessLabel: PIC not defined for this platform"



-- -------------------------------------------------------------------
-- | Says what we we have to add to our 'PIC base register' in order to
--      get the address of a label.

picRelative :: Arch -> OS -> CLabel -> CmmLit

-- Darwin, but not x86_64:
-- The PIC base register points to the PIC base label at the beginning
-- of the current CmmDecl. We just have to use a label difference to
-- get the offset.
-- We have already made sure that all labels that are not from the current
-- module are accessed indirectly ('as' can't calculate differences between
-- undefined labels).
picRelative arch OSDarwin lbl
        | arch /= ArchX86_64
        = CmmLabelDiffOff lbl mkPicBaseLabel 0


-- PowerPC Linux:
-- The PIC base register points to our fake GOT. Use a label difference
-- to get the offset.
-- We have made sure that *everything* is accessed indirectly, so this
-- is only used for offsets from the GOT to symbol pointers inside the
-- GOT.
picRelative ArchPPC os lbl
        | osElfTarget os
        = CmmLabelDiffOff lbl gotLabel 0


-- Most Linux versions:
-- The PIC base register points to the GOT. Use foo@got for symbol
-- pointers, and foo@gotoff for everything else.
-- Linux and Darwin on x86_64:
-- The PIC base register is %rip, we use foo@gotpcrel for symbol pointers,
-- and a GotSymbolOffset label for other things.
-- For reasons of tradition, the symbol offset label is written as a plain label.
picRelative arch os lbl
        | osElfTarget os || (os == OSDarwin && arch == ArchX86_64)
        = let   result
                        | Just (SymbolPtr, lbl') <- dynamicLinkerLabelInfo lbl
                        = CmmLabel $ mkDynamicLinkerLabel GotSymbolPtr lbl'

                        | otherwise
                        = CmmLabel $ mkDynamicLinkerLabel GotSymbolOffset lbl

          in    result

picRelative _ _ _
        = panic "PositionIndependentCode.picRelative undefined for this platform"



--------------------------------------------------------------------------------

needImportedSymbols :: DynFlags -> Arch -> OS -> Bool
needImportedSymbols dflags arch os
        | os    == OSDarwin
        , arch  /= ArchX86_64
        = True

        -- PowerPC Linux: -fPIC or -dynamic
        | osElfTarget os
        , arch  == ArchPPC
        = gopt Opt_PIC dflags || not (gopt Opt_Static dflags)

        -- i386 (and others?): -dynamic but not -fPIC
        | osElfTarget os
        , arch  /= ArchPPC_64
        = not (gopt Opt_Static dflags) && not (gopt Opt_PIC dflags)

        | otherwise
        = False

-- gotLabel
-- The label used to refer to our "fake GOT" from
-- position-independent code.
gotLabel :: CLabel
gotLabel
        -- HACK: this label isn't really foreign
        = mkForeignLabel
                (fsLit ".LCTOC1")
                Nothing ForeignLabelInThisPackage IsData



--------------------------------------------------------------------------------
-- We don't need to declare any offset tables.
-- However, for PIC on x86, we need a small helper function.
pprGotDeclaration :: DynFlags -> Arch -> OS -> SDoc
pprGotDeclaration dflags ArchX86 OSDarwin
        | gopt Opt_PIC dflags
        = vcat [
                ptext (sLit ".section __TEXT,__textcoal_nt,coalesced,no_toc"),
                ptext (sLit ".weak_definition ___i686.get_pc_thunk.ax"),
                ptext (sLit ".private_extern ___i686.get_pc_thunk.ax"),
                ptext (sLit "___i686.get_pc_thunk.ax:"),
                ptext (sLit "\tmovl (%esp), %eax"),
                ptext (sLit "\tret") ]

pprGotDeclaration _ _ OSDarwin
        = empty

-- pprGotDeclaration
-- Output whatever needs to be output once per .s file.
-- The .LCTOC1 label is defined to point 32768 bytes into the table,
-- to make the most of the PPC's 16-bit displacements.
-- Only needed for PIC.
pprGotDeclaration dflags arch os
        | osElfTarget os
        , arch  /= ArchPPC_64
        , not (gopt Opt_PIC dflags)
        = empty

        | osElfTarget os
        , arch  /= ArchPPC_64
        = vcat [
                ptext (sLit ".section \".got2\",\"aw\""),
                ptext (sLit ".LCTOC1 = .+32768") ]

pprGotDeclaration _ _ _
        = panic "pprGotDeclaration: no match"


--------------------------------------------------------------------------------
-- On Darwin, we have to generate our own stub code for lazy binding..
-- For each processor architecture, there are two versions, one for PIC
-- and one for non-PIC.
--
-- Whenever you change something in this assembler output, make sure
-- the splitter in driver/split/ghc-split.lprl recognizes the new output

pprImportedSymbol :: DynFlags -> Platform -> CLabel -> SDoc
pprImportedSymbol dflags platform@(Platform { platformArch = ArchPPC, platformOS = OSDarwin }) importedLbl
        | Just (CodeStub, lbl) <- dynamicLinkerLabelInfo importedLbl
        = case gopt Opt_PIC dflags of
           False ->
            vcat [
                ptext (sLit ".symbol_stub"),
                ptext (sLit "L") <> pprCLabel platform lbl <> ptext (sLit "$stub:"),
                    ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                    ptext (sLit "\tlis r11,ha16(L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr)"),
                    ptext (sLit "\tlwz r12,lo16(L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr)(r11)"),
                    ptext (sLit "\tmtctr r12"),
                    ptext (sLit "\taddi r11,r11,lo16(L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr)"),
                    ptext (sLit "\tbctr")
            ]
           True ->
            vcat [
                ptext (sLit ".section __TEXT,__picsymbolstub1,")
                  <> ptext (sLit "symbol_stubs,pure_instructions,32"),
                ptext (sLit "\t.align 2"),
                ptext (sLit "L") <> pprCLabel platform lbl <> ptext (sLit "$stub:"),
                    ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                    ptext (sLit "\tmflr r0"),
                    ptext (sLit "\tbcl 20,31,L0$") <> pprCLabel platform lbl,
                ptext (sLit "L0$") <> pprCLabel platform lbl <> char ':',
                    ptext (sLit "\tmflr r11"),
                    ptext (sLit "\taddis r11,r11,ha16(L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr-L0$") <> pprCLabel platform lbl <> char ')',
                    ptext (sLit "\tmtlr r0"),
                    ptext (sLit "\tlwzu r12,lo16(L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr-L0$") <> pprCLabel platform lbl
                        <> ptext (sLit ")(r11)"),
                    ptext (sLit "\tmtctr r12"),
                    ptext (sLit "\tbctr")
            ]
          $+$ vcat [
                ptext (sLit ".lazy_symbol_pointer"),
                ptext (sLit "L") <> pprCLabel platform lbl <> ptext (sLit "$lazy_ptr:"),
                ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                ptext (sLit "\t.long dyld_stub_binding_helper")]

        | Just (SymbolPtr, lbl) <- dynamicLinkerLabelInfo importedLbl
        = vcat [
                ptext (sLit ".non_lazy_symbol_pointer"),
                char 'L' <> pprCLabel platform lbl <> ptext (sLit "$non_lazy_ptr:"),
                ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                ptext (sLit "\t.long\t0")]

        | otherwise
        = empty


pprImportedSymbol dflags platform@(Platform { platformArch = ArchX86, platformOS = OSDarwin }) importedLbl
        | Just (CodeStub, lbl) <- dynamicLinkerLabelInfo importedLbl
        = case gopt Opt_PIC dflags of
           False ->
            vcat [
                ptext (sLit ".symbol_stub"),
                ptext (sLit "L") <> pprCLabel platform lbl <> ptext (sLit "$stub:"),
                    ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                    ptext (sLit "\tjmp *L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr"),
                ptext (sLit "L") <> pprCLabel platform lbl
                    <> ptext (sLit "$stub_binder:"),
                    ptext (sLit "\tpushl $L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr"),
                    ptext (sLit "\tjmp dyld_stub_binding_helper")
            ]
           True ->
            vcat [
                ptext (sLit ".section __TEXT,__picsymbolstub2,")
                    <> ptext (sLit "symbol_stubs,pure_instructions,25"),
                ptext (sLit "L") <> pprCLabel platform lbl <> ptext (sLit "$stub:"),
                    ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                    ptext (sLit "\tcall ___i686.get_pc_thunk.ax"),
                ptext (sLit "1:"),
                    ptext (sLit "\tmovl L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr-1b(%eax),%edx"),
                    ptext (sLit "\tjmp *%edx"),
                ptext (sLit "L") <> pprCLabel platform lbl
                    <> ptext (sLit "$stub_binder:"),
                    ptext (sLit "\tlea L") <> pprCLabel platform lbl
                        <> ptext (sLit "$lazy_ptr-1b(%eax),%eax"),
                    ptext (sLit "\tpushl %eax"),
                    ptext (sLit "\tjmp dyld_stub_binding_helper")
            ]
          $+$ vcat [        ptext (sLit ".section __DATA, __la_sym_ptr")
                    <> (if gopt Opt_PIC dflags then int 2 else int 3)
                    <> ptext (sLit ",lazy_symbol_pointers"),
                ptext (sLit "L") <> pprCLabel platform lbl <> ptext (sLit "$lazy_ptr:"),
                    ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                    ptext (sLit "\t.long L") <> pprCLabel platform lbl
                    <> ptext (sLit "$stub_binder")]

        | Just (SymbolPtr, lbl) <- dynamicLinkerLabelInfo importedLbl
        = vcat [
                ptext (sLit ".non_lazy_symbol_pointer"),
                char 'L' <> pprCLabel platform lbl <> ptext (sLit "$non_lazy_ptr:"),
                ptext (sLit "\t.indirect_symbol") <+> pprCLabel platform lbl,
                ptext (sLit "\t.long\t0")]

        | otherwise
        = empty


pprImportedSymbol _ (Platform { platformOS = OSDarwin }) _
        = empty


-- ELF / Linux
--
-- In theory, we don't need to generate any stubs or symbol pointers
-- by hand for Linux.
--
-- Reality differs from this in two areas.
--
-- 1) If we just use a dynamically imported symbol directly in a read-only
--    section of the main executable (as GCC does), ld generates R_*_COPY
--    relocations, which are fundamentally incompatible with reversed info
--    tables. Therefore, we need a table of imported addresses in a writable
--    section.
--    The "official" GOT mechanism (label@got) isn't intended to be used
--    in position dependent code, so we have to create our own "fake GOT"
--    when not Opt_PIC && not (gopt Opt_Static dflags).
--
-- 2) PowerPC Linux is just plain broken.
--    While it's theoretically possible to use GOT offsets larger
--    than 16 bit, the standard crt*.o files don't, which leads to
--    linker errors as soon as the GOT size exceeds 16 bit.
--    Also, the assembler doesn't support @gotoff labels.
--    In order to be able to use a larger GOT, we have to circumvent the
--    entire GOT mechanism and do it ourselves (this is also what GCC does).


-- When needImportedSymbols is defined,
-- the NCG will keep track of all DynamicLinkerLabels it uses
-- and output each of them using pprImportedSymbol.

pprImportedSymbol _ platform@(Platform { platformArch = ArchPPC_64 }) _
        | osElfTarget (platformOS platform)
        = empty

pprImportedSymbol dflags platform importedLbl
        | osElfTarget (platformOS platform)
        = case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> let symbolSize = case wordWidth dflags of
                         W32 -> sLit "\t.long"
                         W64 -> sLit "\t.quad"
                         _ -> panic "Unknown wordRep in pprImportedSymbol"

                 in vcat [
                      ptext (sLit ".section \".got2\", \"aw\""),
                      ptext (sLit ".LC_") <> pprCLabel platform lbl <> char ':',
                      ptext symbolSize <+> pprCLabel platform lbl ]

            -- PLT code stubs are generated automatically by the dynamic linker.
            _ -> empty

pprImportedSymbol _ _ _
        = panic "PIC.pprImportedSymbol: no match"

--------------------------------------------------------------------------------
-- Generate code to calculate the address that should be put in the
-- PIC base register.
-- This is called by MachCodeGen for every CmmProc that accessed the
-- PIC base register. It adds the appropriate instructions to the
-- top of the CmmProc.

-- It is assumed that the first NatCmmDecl in the input list is a Proc
-- and the rest are CmmDatas.

-- Darwin is simple: just fetch the address of a local label.
-- The FETCHPC pseudo-instruction is expanded to multiple instructions
-- during pretty-printing so that we don't have to deal with the
-- local label:

-- PowerPC version:
--          bcl 20,31,1f.
--      1:  mflr picReg

-- i386 version:
--          call 1f
--      1:  popl %picReg



-- Get a pointer to our own fake GOT, which is defined on a per-module basis.
-- This is exactly how GCC does it, and it's quite horrible:
-- We first fetch the address of a local label (mkPicBaseLabel).
-- Then we add a 16-bit offset to that to get the address of a .long that we
-- define in .text space right next to the proc. This .long literal contains
-- the (32-bit) offset from our local label to our global offset table
-- (.LCTOC1 aka gotOffLabel).

initializePicBase_ppc
        :: Arch -> OS -> Reg
        -> [NatCmmDecl CmmStatics PPC.Instr]
        -> NatM [NatCmmDecl CmmStatics PPC.Instr]

initializePicBase_ppc ArchPPC os picReg
    (CmmProc info lab live (ListGraph blocks) : statics)
    | osElfTarget os
    = do
        dflags <- getDynFlags
        gotOffLabel <- getNewLabelNat
        tmp <- getNewRegNat $ intSize (wordWidth dflags)
        let
            gotOffset = CmmData Text $ Statics gotOffLabel [
                            CmmStaticLit (CmmLabelDiffOff gotLabel
                                                          mkPicBaseLabel
                                                          0)
                        ]
            offsetToOffset
                        = PPC.ImmConstantDiff
                                (PPC.ImmCLbl gotOffLabel)
                                (PPC.ImmCLbl mkPicBaseLabel)

            BasicBlock bID insns
                        = head blocks

            b' = BasicBlock bID (PPC.FETCHPC picReg
                               : PPC.LD PPC.archWordSize tmp
                                    (PPC.AddrRegImm picReg offsetToOffset)
                               : PPC.ADD picReg picReg (PPC.RIReg tmp)
                               : insns)

        return (CmmProc info lab live (ListGraph (b' : tail blocks)) : gotOffset : statics)

initializePicBase_ppc ArchPPC OSDarwin picReg
        (CmmProc info lab live (ListGraph blocks) : statics)
        = return (CmmProc info lab live (ListGraph (b':tail blocks)) : statics)

        where   BasicBlock bID insns = head blocks
                b' = BasicBlock bID (PPC.FETCHPC picReg : insns)


initializePicBase_ppc _ _ _ _
        = panic "initializePicBase_ppc: not needed"


-- We cheat a bit here by defining a pseudo-instruction named FETCHGOT
-- which pretty-prints as:
--              call 1f
-- 1:           popl %picReg
--              addl __GLOBAL_OFFSET_TABLE__+.-1b, %picReg
-- (See PprMach.lhs)

initializePicBase_x86
        :: Arch -> OS -> Reg
        -> [NatCmmDecl (Alignment, CmmStatics) X86.Instr]
        -> NatM [NatCmmDecl (Alignment, CmmStatics) X86.Instr]

initializePicBase_x86 ArchX86 os picReg
        (CmmProc info lab live (ListGraph blocks) : statics)
    | osElfTarget os
    = return (CmmProc info lab live (ListGraph blocks') : statics)
    where blocks' = case blocks of
                     [] -> []
                     (b:bs) -> fetchGOT b : map maybeFetchGOT bs

          -- we want to add a FETCHGOT instruction to the beginning of
          -- every block that is an entry point, which corresponds to
          -- the blocks that have entries in the info-table mapping.
          maybeFetchGOT b@(BasicBlock bID _)
            | bID `mapMember` info = fetchGOT b
            | otherwise            = b

          fetchGOT (BasicBlock bID insns) =
             BasicBlock bID (X86.FETCHGOT picReg : insns)

initializePicBase_x86 ArchX86 OSDarwin picReg
        (CmmProc info lab live (ListGraph blocks) : statics)
        = return (CmmProc info lab live (ListGraph blocks') : statics)

    where blocks' = case blocks of
                     [] -> []
                     (b:bs) -> fetchPC b : map maybeFetchPC bs

          maybeFetchPC b@(BasicBlock bID _)
            | bID `mapMember` info = fetchPC b
            | otherwise            = b

          fetchPC (BasicBlock bID insns) =
             BasicBlock bID (X86.FETCHPC picReg : insns)

initializePicBase_x86 _ _ _ _
        = panic "initializePicBase_x86: not needed"


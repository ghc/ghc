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
        CmmMakeDynamicReferenceM(..),
        ReferenceKind(..),
        needImportedSymbols,
        pprImportedSymbol,
        pprGotDeclaration,

        initializePicBase_ppc,
        initializePicBase_x86
)

where

import GhcPrelude

import qualified PPC.Instr      as PPC
import qualified PPC.Regs       as PPC

import qualified X86.Instr      as X86

import Platform
import Instruction
import Reg
import NCGMonad


import Hoopl.Collections
import Cmm
import CLabel           ( CLabel, ForeignLabelSource(..), pprCLabel,
                          mkDynamicLinkerLabel, DynamicLinkerLabelInfo(..),
                          dynamicLinkerLabelInfo, mkPicBaseLabel,
                          labelDynamic, externallyVisibleCLabel )

import CLabel           ( mkForeignLabel )


import BasicTypes
import Module

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

class Monad m => CmmMakeDynamicReferenceM m where
    addImport :: CLabel -> m ()
    getThisModule :: m Module

instance CmmMakeDynamicReferenceM NatM where
    addImport = addImportNat
    getThisModule = getThisModuleNat

cmmMakeDynamicReference
  :: CmmMakeDynamicReferenceM m
  => DynFlags
  -> ReferenceKind     -- whether this is the target of a jump
  -> CLabel            -- the label
  -> m CmmExpr

cmmMakeDynamicReference dflags referenceKind lbl
  | Just _ <- dynamicLinkerLabelInfo lbl
  = return $ CmmLit $ CmmLabel lbl   -- already processed it, pass through

  | otherwise
  = do this_mod <- getThisModule
       case howToAccessLabel
                dflags
                (platformArch $ targetPlatform dflags)
                (platformOS   $ targetPlatform dflags)
                this_mod
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

        | OSAIX <- platformOS $ targetPlatform dflags
        = CmmMachOp (MO_Add W32)
                [ CmmReg (CmmGlobal PicBaseReg)
                , CmmLit $ picRelative dflags
                                (platformArch   $ targetPlatform dflags)
                                (platformOS     $ targetPlatform dflags)
                                lbl ]

        -- both ABI versions default to medium code model
        | ArchPPC_64 _ <- platformArch $ targetPlatform dflags
        = CmmMachOp (MO_Add W32) -- code model medium
                [ CmmReg (CmmGlobal PicBaseReg)
                , CmmLit $ picRelative dflags
                                (platformArch   $ targetPlatform dflags)
                                (platformOS     $ targetPlatform dflags)
                                lbl ]

        | (positionIndependent dflags || gopt Opt_ExternalDynamicRefs dflags)
            && absoluteLabel lbl
        = CmmMachOp (MO_Add (wordWidth dflags))
                [ CmmReg (CmmGlobal PicBaseReg)
                , CmmLit $ picRelative dflags
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
        :: DynFlags -> Arch -> OS -> Module -> ReferenceKind -> CLabel -> LabelAccessStyle


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
-- If not compiling with -dynamic we assume that all our code will be linked
-- into the same .exe file. In this case we always access symbols directly,
-- and never use __imp_SYMBOL.
--
howToAccessLabel dflags _ OSMinGW32 this_mod _ lbl

        -- Assume all symbols will be in the same PE, so just access them directly.
        | not (gopt Opt_ExternalDynamicRefs dflags)
        = AccessDirectly

        -- If the target symbol is in another PE we need to access it via the
        --      appropriate __imp_SYMBOL pointer.
        | labelDynamic dflags this_mod lbl
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
howToAccessLabel dflags arch OSDarwin this_mod DataReference lbl
        -- data access to a dynamic library goes via a symbol pointer
        | labelDynamic dflags this_mod lbl
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
        , positionIndependent dflags && externallyVisibleCLabel lbl
        = AccessViaSymbolPtr

        | otherwise
        = AccessDirectly

howToAccessLabel dflags arch OSDarwin this_mod JumpReference lbl
        -- dyld code stubs don't work for tailcalls because the
        -- stack alignment is only right for regular calls.
        -- Therefore, we have to go via a symbol pointer:
        | arch == ArchX86 || arch == ArchX86_64
        , labelDynamic dflags this_mod lbl
        = AccessViaSymbolPtr


howToAccessLabel dflags arch OSDarwin this_mod _ lbl
        -- Code stubs are the usual method of choice for imported code;
        -- not needed on x86_64 because Apple's new linker, ld64, generates
        -- them automatically.
        | arch /= ArchX86_64
        , labelDynamic dflags this_mod lbl
        = AccessViaStub

        | otherwise
        = AccessDirectly


----------------------------------------------------------------------------
-- AIX

-- quite simple (for now)
howToAccessLabel _dflags _arch OSAIX _this_mod kind _lbl
        = case kind of
            DataReference -> AccessViaSymbolPtr
            CallReference -> AccessDirectly
            JumpReference -> AccessDirectly

-- ELF (Linux)
--
-- ELF tries to pretend to the main application code that dynamic linking does
-- not exist. While this may sound convenient, it tends to mess things up in
-- very bad ways, so we have to be careful when we generate code for a non-PIE
-- main program (-dynamic but no -fPIC).
--
-- Indirect access is required for references to imported symbols
-- from position independent code. It is also required from the main program
-- when dynamic libraries containing Haskell code are used.

howToAccessLabel _ (ArchPPC_64 _) os _ kind _
        | osElfTarget os
        = case kind of
          -- ELF PPC64 (powerpc64-linux), AIX, MacOS 9, BeOS/PPC
          DataReference -> AccessViaSymbolPtr
          -- RTLD does not generate stubs for function descriptors
          -- in tail calls. Create a symbol pointer and generate
          -- the code to load the function descriptor at the call site.
          JumpReference -> AccessViaSymbolPtr
          -- regular calls are handled by the runtime linker
          _             -> AccessDirectly

howToAccessLabel dflags _ os _ _ _
        -- no PIC -> the dynamic linker does everything for us;
        --           if we don't dynamically link to Haskell code,
        --           it actually manages to do so without messing things up.
        | osElfTarget os
        , not (positionIndependent dflags) &&
          not (gopt Opt_ExternalDynamicRefs dflags)
        = AccessDirectly

howToAccessLabel dflags arch os this_mod DataReference lbl
        | osElfTarget os
        = case () of
            -- A dynamic label needs to be accessed via a symbol pointer.
          _ | labelDynamic dflags this_mod lbl
            -> AccessViaSymbolPtr

            -- For PowerPC32 -fPIC, we have to access even static data
            -- via a symbol pointer (see below for an explanation why
            -- PowerPC32 Linux is especially broken).
            | arch == ArchPPC
            , positionIndependent dflags
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

howToAccessLabel dflags arch os this_mod CallReference lbl
        | osElfTarget os
        , labelDynamic dflags this_mod lbl && not (positionIndependent dflags)
        = AccessDirectly

        | osElfTarget os
        , arch /= ArchX86
        , labelDynamic dflags this_mod lbl
        , positionIndependent dflags
        = AccessViaStub

howToAccessLabel dflags _ os this_mod _ lbl
        | osElfTarget os
        = if labelDynamic dflags this_mod lbl
            then AccessViaSymbolPtr
            else AccessDirectly

-- all other platforms
howToAccessLabel dflags _ _ _ _ _
        | not (positionIndependent dflags)
        = AccessDirectly

        | otherwise
        = panic "howToAccessLabel: PIC not defined for this platform"



-- -------------------------------------------------------------------
-- | Says what we have to add to our 'PIC base register' in order to
--      get the address of a label.

picRelative :: DynFlags -> Arch -> OS -> CLabel -> CmmLit

-- Darwin, but not x86_64:
-- The PIC base register points to the PIC base label at the beginning
-- of the current CmmDecl. We just have to use a label difference to
-- get the offset.
-- We have already made sure that all labels that are not from the current
-- module are accessed indirectly ('as' can't calculate differences between
-- undefined labels).
picRelative dflags arch OSDarwin lbl
        | arch /= ArchX86_64
        = CmmLabelDiffOff lbl mkPicBaseLabel 0 (wordWidth dflags)

-- On AIX we use an indirect local TOC anchored by 'gotLabel'.
-- This way we use up only one global TOC entry per compilation-unit
-- (this is quite similiar to GCC's @-mminimal-toc@ compilation mode)
picRelative dflags _ OSAIX lbl
        = CmmLabelDiffOff lbl gotLabel 0 (wordWidth dflags)

-- PowerPC Linux:
-- The PIC base register points to our fake GOT. Use a label difference
-- to get the offset.
-- We have made sure that *everything* is accessed indirectly, so this
-- is only used for offsets from the GOT to symbol pointers inside the
-- GOT.
picRelative dflags ArchPPC os lbl
        | osElfTarget os
        = CmmLabelDiffOff lbl gotLabel 0 (wordWidth dflags)


-- Most Linux versions:
-- The PIC base register points to the GOT. Use foo@got for symbol
-- pointers, and foo@gotoff for everything else.
-- Linux and Darwin on x86_64:
-- The PIC base register is %rip, we use foo@gotpcrel for symbol pointers,
-- and a GotSymbolOffset label for other things.
-- For reasons of tradition, the symbol offset label is written as a plain label.
picRelative _ arch os lbl
        | osElfTarget os || (os == OSDarwin && arch == ArchX86_64)
        = let   result
                        | Just (SymbolPtr, lbl') <- dynamicLinkerLabelInfo lbl
                        = CmmLabel $ mkDynamicLinkerLabel GotSymbolPtr lbl'

                        | otherwise
                        = CmmLabel $ mkDynamicLinkerLabel GotSymbolOffset lbl

          in    result

picRelative _ _ _ _
        = panic "PositionIndependentCode.picRelative undefined for this platform"



--------------------------------------------------------------------------------

needImportedSymbols :: DynFlags -> Arch -> OS -> Bool
needImportedSymbols dflags arch os
        | os    == OSDarwin
        , arch  /= ArchX86_64
        = True

        | os    == OSAIX
        = True

        -- PowerPC Linux: -fPIC or -dynamic
        | osElfTarget os
        , arch  == ArchPPC
        = positionIndependent dflags || gopt Opt_ExternalDynamicRefs dflags

        -- PowerPC 64 Linux: always
        | osElfTarget os
        , arch == ArchPPC_64 ELF_V1 || arch == ArchPPC_64 ELF_V2
        = True

        -- i386 (and others?): -dynamic but not -fPIC
        | osElfTarget os
        , arch /= ArchPPC_64 ELF_V1 && arch /= ArchPPC_64 ELF_V2
        = gopt Opt_ExternalDynamicRefs dflags &&
          not (positionIndependent dflags)

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
        | positionIndependent dflags
        = vcat [
                text ".section __TEXT,__textcoal_nt,coalesced,no_toc",
                text ".weak_definition ___i686.get_pc_thunk.ax",
                text ".private_extern ___i686.get_pc_thunk.ax",
                text "___i686.get_pc_thunk.ax:",
                text "\tmovl (%esp), %eax",
                text "\tret" ]

pprGotDeclaration _ _ OSDarwin
        = empty

-- Emit XCOFF TOC section
pprGotDeclaration _ _ OSAIX
        = vcat $ [ text ".toc"
                 , text ".tc ghc_toc_table[TC],.LCTOC1"
                 , text ".csect ghc_toc_table[RW]"
                   -- See Note [.LCTOC1 in PPC PIC code]
                 , text ".set .LCTOC1,$+0x8000"
                 ]


-- PPC 64 ELF v1 needs a Table Of Contents (TOC)
pprGotDeclaration _ (ArchPPC_64 ELF_V1) _
        = text ".section \".toc\",\"aw\""
-- In ELF v2 we also need to tell the assembler that we want ABI
-- version 2. This would normally be done at the top of the file
-- right after a file directive, but I could not figure out how
-- to do that.
pprGotDeclaration _ (ArchPPC_64 ELF_V2) _
        = vcat [ text ".abiversion 2",
                 text ".section \".toc\",\"aw\""
               ]

-- Emit GOT declaration
-- Output whatever needs to be output once per .s file.
pprGotDeclaration dflags arch os
        | osElfTarget os
        , arch /= ArchPPC_64 ELF_V1 && arch /= ArchPPC_64 ELF_V2
        , not (positionIndependent dflags)
        = empty

        | osElfTarget os
        , arch /= ArchPPC_64 ELF_V1 && arch /= ArchPPC_64 ELF_V2
        = vcat [
                -- See Note [.LCTOC1 in PPC PIC code]
                text ".section \".got2\",\"aw\"",
                text ".LCTOC1 = .+32768" ]

pprGotDeclaration _ _ _
        = panic "pprGotDeclaration: no match"


--------------------------------------------------------------------------------
-- On Darwin, we have to generate our own stub code for lazy binding..
-- For each processor architecture, there are two versions, one for PIC
-- and one for non-PIC.
--
-- Whenever you change something in this assembler output, make sure
-- the splitter in driver/split/ghc-split.pl recognizes the new output

pprImportedSymbol :: DynFlags -> Platform -> CLabel -> SDoc
pprImportedSymbol dflags platform@(Platform { platformArch = ArchX86, platformOS = OSDarwin }) importedLbl
        | Just (CodeStub, lbl) <- dynamicLinkerLabelInfo importedLbl
        = case positionIndependent dflags of
           False ->
            vcat [
                text ".symbol_stub",
                text "L" <> pprCLabel platform lbl <> ptext (sLit "$stub:"),
                    text "\t.indirect_symbol" <+> pprCLabel platform lbl,
                    text "\tjmp *L" <> pprCLabel platform lbl
                        <> text "$lazy_ptr",
                text "L" <> pprCLabel platform lbl
                    <> text "$stub_binder:",
                    text "\tpushl $L" <> pprCLabel platform lbl
                        <> text "$lazy_ptr",
                    text "\tjmp dyld_stub_binding_helper"
            ]
           True ->
            vcat [
                text ".section __TEXT,__picsymbolstub2,"
                    <> text "symbol_stubs,pure_instructions,25",
                text "L" <> pprCLabel platform lbl <> ptext (sLit "$stub:"),
                    text "\t.indirect_symbol" <+> pprCLabel platform lbl,
                    text "\tcall ___i686.get_pc_thunk.ax",
                text "1:",
                    text "\tmovl L" <> pprCLabel platform lbl
                        <> text "$lazy_ptr-1b(%eax),%edx",
                    text "\tjmp *%edx",
                text "L" <> pprCLabel platform lbl
                    <> text "$stub_binder:",
                    text "\tlea L" <> pprCLabel platform lbl
                        <> text "$lazy_ptr-1b(%eax),%eax",
                    text "\tpushl %eax",
                    text "\tjmp dyld_stub_binding_helper"
            ]
          $+$ vcat [        text ".section __DATA, __la_sym_ptr"
                    <> (if positionIndependent dflags then int 2 else int 3)
                    <> text ",lazy_symbol_pointers",
                text "L" <> pprCLabel platform lbl <> ptext (sLit "$lazy_ptr:"),
                    text "\t.indirect_symbol" <+> pprCLabel platform lbl,
                    text "\t.long L" <> pprCLabel platform lbl
                    <> text "$stub_binder"]

        | Just (SymbolPtr, lbl) <- dynamicLinkerLabelInfo importedLbl
        = vcat [
                text ".non_lazy_symbol_pointer",
                char 'L' <> pprCLabel platform lbl <> text "$non_lazy_ptr:",
                text "\t.indirect_symbol" <+> pprCLabel platform lbl,
                text "\t.long\t0"]

        | otherwise
        = empty


pprImportedSymbol _ (Platform { platformOS = OSDarwin }) _
        = empty

-- XCOFF / AIX
--
-- Similiar to PPC64 ELF v1, there's dedicated TOC register (r2). To
-- workaround the limitation of a global TOC we use an indirect TOC
-- with the label `ghc_toc_table`.
--
-- See also GCC's `-mminimal-toc` compilation mode or
-- http://www.ibm.com/developerworks/rational/library/overview-toc-aix/
--
-- NB: No DSO-support yet

pprImportedSymbol _ platform@(Platform { platformOS = OSAIX }) importedLbl
        = case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> vcat [
                   text "LC.." <> pprCLabel platform lbl <> char ':',
                   text "\t.long" <+> pprCLabel platform lbl ]
            _ -> empty

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
--    when not Opt_PIC && WayDyn `elem` ways dflags.
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

pprImportedSymbol _ platform@(Platform { platformArch = ArchPPC_64 _ })
                  importedLbl
        | osElfTarget (platformOS platform)
        = case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> vcat [
                   text ".section \".toc\", \"aw\"",
                   text ".LC_" <> pprCLabel platform lbl <> char ':',
                   text "\t.quad" <+> pprCLabel platform lbl ]
            _ -> empty

pprImportedSymbol dflags platform importedLbl
        | osElfTarget (platformOS platform)
        = case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> let symbolSize = case wordWidth dflags of
                         W32 -> sLit "\t.long"
                         W64 -> sLit "\t.quad"
                         _ -> panic "Unknown wordRep in pprImportedSymbol"

                 in vcat [
                      text ".section \".got2\", \"aw\"",
                      text ".LC_" <> pprCLabel platform lbl <> char ':',
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
-- This is exactly how GCC does it in linux.

initializePicBase_ppc
        :: Arch -> OS -> Reg
        -> [NatCmmDecl CmmStatics PPC.Instr]
        -> NatM [NatCmmDecl CmmStatics PPC.Instr]

initializePicBase_ppc ArchPPC os picReg
    (CmmProc info lab live (ListGraph blocks) : statics)
    | osElfTarget os
    = do
        let
            gotOffset = PPC.ImmConstantDiff
                                (PPC.ImmCLbl gotLabel)
                                (PPC.ImmCLbl mkPicBaseLabel)

            blocks' = case blocks of
                       [] -> []
                       (b:bs) -> fetchPC b : map maybeFetchPC bs

            maybeFetchPC b@(BasicBlock bID _)
              | bID `mapMember` info = fetchPC b
              | otherwise            = b

            -- GCC does PIC prologs thusly:
            --     bcl 20,31,.L1
            -- .L1:
            --     mflr 30
            --     addis 30,30,.LCTOC1-.L1@ha
            --     addi 30,30,.LCTOC1-.L1@l
            -- TODO: below we use it over temporary register,
            -- it can and should be optimised by picking
            -- correct PIC reg.
            fetchPC (BasicBlock bID insns) =
              BasicBlock bID (PPC.FETCHPC picReg
                              : PPC.ADDIS picReg picReg (PPC.HA gotOffset)
                              : PPC.ADD picReg picReg
                                        (PPC.RIImm (PPC.LO gotOffset))
                              : PPC.MR PPC.r30 picReg
                              : insns)

        return (CmmProc info lab live (ListGraph blocks') : statics)

-------------------------------------------------------------------------
-- Load TOC into register 2
-- PowerPC 64-bit ELF ABI 2.0 requires the address of the callee
-- in register 12.
-- We pass the label to FETCHTOC and create a .localentry too.
-- TODO: Explain this better and refer to ABI spec!
{-
We would like to do approximately this, but spill slot allocation
might be added before the first BasicBlock. That violates the ABI.

For now we will emit the prologue code in the pretty printer,
which is also what we do for ELF v1.
initializePicBase_ppc (ArchPPC_64 ELF_V2) OSLinux picReg
        (CmmProc info lab live (ListGraph (entry:blocks)) : statics)
        = do
           bID <-getUniqueM
           return (CmmProc info lab live (ListGraph (b':entry:blocks))
                                         : statics)
        where   BasicBlock entryID _ = entry
                b' = BasicBlock bID [PPC.FETCHTOC picReg lab,
                                     PPC.BCC PPC.ALWAYS entryID]
-}

initializePicBase_ppc _ _ _ _
        = panic "initializePicBase_ppc: not needed"


-- We cheat a bit here by defining a pseudo-instruction named FETCHGOT
-- which pretty-prints as:
--              call 1f
-- 1:           popl %picReg
--              addl __GLOBAL_OFFSET_TABLE__+.-1b, %picReg
-- (See PprMach.hs)

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
        (CmmProc info lab live (ListGraph (entry:blocks)) : statics)
        = return (CmmProc info lab live (ListGraph (block':blocks)) : statics)

    where BasicBlock bID insns = entry
          block' = BasicBlock bID (X86.FETCHPC picReg : insns)

initializePicBase_x86 _ _ _ _
        = panic "initializePicBase_x86: not needed"


{-
  This module handles generation of position independent code and
  dynamic-linking related issues for the native code generator.

  This depends on both the architecture and OS, so we define it here
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

module GHC.CmmToAsm.PIC (
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

import GHC.Prelude

import qualified GHC.CmmToAsm.PPC.Instr as PPC
import qualified GHC.CmmToAsm.PPC.Regs  as PPC
import qualified GHC.CmmToAsm.X86.Instr as X86

import GHC.Platform
import GHC.Platform.Reg
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types


import GHC.Cmm.Dataflow.Label.NonDet (mapMember)
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Utils (cmmLoadBWord)

import GHC.Types.Basic

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.FastString



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

instance CmmMakeDynamicReferenceM NatM where
    addImport = addImportNat

cmmMakeDynamicReference
  :: CmmMakeDynamicReferenceM m
  => NCGConfig
  -> ReferenceKind     -- whether this is the target of a jump
  -> CLabel            -- the label
  -> m CmmExpr

cmmMakeDynamicReference config referenceKind lbl
  | Just _ <- dynamicLinkerLabelInfo lbl
  = return $ CmmLit $ CmmLabel lbl   -- already processed it, pass through

  | otherwise
  = do let platform = ncgPlatform config
       case howToAccessLabel
                config
                (platformArch platform)
                (platformOS   platform)
                referenceKind lbl of

        AccessViaStub -> do
              let stub = mkDynamicLinkerLabel CodeStub lbl
              addImport stub
              return $ CmmLit $ CmmLabel stub

        -- GOT relative loads work differently on AArch64.  We don't do two
        -- step loads. The got symbol is loaded directly, and not through an
        -- additional load. Thus we do not need the CmmLoad decoration we have
        -- on other platforms.
        AccessViaSymbolPtr | ArchAArch64 <- platformArch platform -> do
              let symbolPtr = mkDynamicLinkerLabel SymbolPtr lbl
              addImport symbolPtr
              return $ cmmMakePicReference config symbolPtr

        AccessViaSymbolPtr -> do
              let symbolPtr = mkDynamicLinkerLabel SymbolPtr lbl
              addImport symbolPtr
              return $ cmmLoadBWord platform (cmmMakePicReference config symbolPtr)

        AccessDirectly -> case referenceKind of
                -- for data, we might have to make some calculations:
              DataReference -> return $ cmmMakePicReference config lbl
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

cmmMakePicReference :: NCGConfig -> CLabel -> CmmExpr
cmmMakePicReference config lbl
  -- Windows doesn't need PIC,
  -- everything gets relocated at runtime
  | OSMinGW32 <- platformOS platform
  = CmmLit $ CmmLabel lbl

  -- no pic base reg on AArch64, however indicate this symbol should go through
  -- the global offset table (GOT).
  | ArchAArch64 <- platformArch platform
  = CmmLit $ CmmLabel lbl

  | OSAIX <- platformOS platform
  = CmmMachOp (MO_Add W32)
          [ CmmReg (CmmGlobal $ GlobalRegUse PicBaseReg (bWord platform))
          , CmmLit $ picRelative (wordWidth platform)
                          (platformArch platform)
                          (platformOS   platform)
                          lbl ]

  -- both ABI versions default to medium code model
  | ArchPPC_64 _ <- platformArch platform
  = CmmMachOp (MO_Add W32) -- code model medium
          [ CmmReg (CmmGlobal $ GlobalRegUse PicBaseReg (bWord platform))
          , CmmLit $ picRelative (wordWidth platform)
                          (platformArch platform)
                          (platformOS   platform)
                          lbl ]

  | (ncgPIC config || ncgExternalDynamicRefs config)
      && absoluteLabel lbl
  = CmmMachOp (MO_Add (wordWidth platform))
          [ CmmReg (CmmGlobal $ GlobalRegUse PicBaseReg (bWord platform))
          , CmmLit $ picRelative (wordWidth platform)
                          (platformArch platform)
                          (platformOS   platform)
                          lbl ]

  | otherwise
  = CmmLit $ CmmLabel lbl
  where
    platform = ncgPlatform config



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

-- | Helper to check whether the data resides in a DLL or not, see @labelDynamic@
ncgLabelDynamic :: NCGConfig -> CLabel -> Bool
ncgLabelDynamic config = labelDynamic (ncgThisModule config)
                                      (ncgPlatform config)
                                      (ncgExternalDynamicRefs config)


-- We have to decide which labels need to be accessed
-- indirectly or via a piece of stub code.
data LabelAccessStyle
        = AccessViaStub
        | AccessViaSymbolPtr
        | AccessDirectly

howToAccessLabel :: NCGConfig -> Arch -> OS -> ReferenceKind -> CLabel -> LabelAccessStyle

-- Windows
-- In Windows speak, a "module" is a set of objects linked into the
-- same Portable Executable (PE) file. (both .exe and .dll files are PEs).
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
howToAccessLabel config _arch OSMinGW32 _kind lbl

        -- Assume all symbols will be in the same PE, so just access them directly.
        | not (ncgExternalDynamicRefs config)
        = AccessDirectly

        -- If the target symbol is in another PE we need to access it via the
        --      appropriate __imp_SYMBOL pointer.
        | ncgLabelDynamic config lbl
        = AccessViaSymbolPtr

        -- Target symbol is in the same PE as the caller, so just access it directly.
        | otherwise
        = AccessDirectly

-- On AArch64, relocations for JUMP and CALL will be emitted with 26bits, this
-- is enough for ~64MB of range. Anything else will need to go through a veneer,
-- which is the job of the linker to build.  We might only want to lookup
-- Data References through the GOT.
howToAccessLabel config ArchAArch64 _os _kind lbl
        | not (ncgExternalDynamicRefs config)
        = AccessDirectly

        | ncgLabelDynamic config lbl
        = AccessViaSymbolPtr

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
howToAccessLabel config arch OSDarwin DataReference lbl
        -- data access to a dynamic library goes via a symbol pointer
        | ncgLabelDynamic config lbl
        = AccessViaSymbolPtr

        -- when generating PIC code, all cross-module data references must
        -- must go via a symbol pointer, too, because the assembler
        -- cannot generate code for a label difference where one
        -- label is undefined. Doesn't apply to x86_64 (why?).
        | arch /= ArchX86_64
        , not (isLocalCLabel (ncgThisModule config) lbl)
        , ncgPIC config
        , externallyVisibleCLabel lbl
        = AccessViaSymbolPtr

        | otherwise
        = AccessDirectly

howToAccessLabel config _ OSDarwin JumpReference lbl
        -- dyld code stubs don't work for tailcalls because the
        -- stack alignment is only right for regular calls.
        -- Therefore, we have to go via a symbol pointer:
        | ncgLabelDynamic config lbl
        = AccessViaSymbolPtr


howToAccessLabel _ _ OSDarwin _ _
        = AccessDirectly

----------------------------------------------------------------------------
-- AIX

-- quite simple (for now)
howToAccessLabel _config _arch OSAIX kind _lbl
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

howToAccessLabel _config (ArchPPC_64 _) os kind _lbl
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

howToAccessLabel config _arch os _kind _lbl
        -- no PIC -> the dynamic linker does everything for us;
        --           if we don't dynamically link to Haskell code,
        --           it actually manages to do so without messing things up.
        | osElfTarget os
        , not (ncgPIC config) &&
          not (ncgExternalDynamicRefs config)
        = AccessDirectly

howToAccessLabel config arch os DataReference lbl
        | osElfTarget os
        = case () of
            -- A dynamic label needs to be accessed via a symbol pointer.
          _ | ncgLabelDynamic config lbl
            -> AccessViaSymbolPtr

            -- For PowerPC32 -fPIC, we have to access even static data
            -- via a symbol pointer (see below for an explanation why
            -- PowerPC32 Linux is especially broken).
            | arch == ArchPPC
            , ncgPIC config
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

howToAccessLabel config arch os CallReference lbl
        | osElfTarget os
        , ncgLabelDynamic config lbl
        , not (ncgPIC config)
        = AccessDirectly

        | osElfTarget os
        , arch /= ArchX86
        , ncgLabelDynamic config lbl
        , ncgPIC config
        = AccessViaStub

howToAccessLabel config _arch os _kind lbl
        | osElfTarget os
        = if ncgLabelDynamic config lbl
            then AccessViaSymbolPtr
            else AccessDirectly

-- all other platforms
howToAccessLabel config _arch _os _kind _lbl
        | not (ncgPIC config)
        = AccessDirectly

        | otherwise
        = panic "howToAccessLabel: PIC not defined for this platform"



-- -------------------------------------------------------------------
-- | Says what we have to add to our 'PIC base register' in order to
--      get the address of a label.

picRelative :: Width -> Arch -> OS -> CLabel -> CmmLit

-- Darwin, but not x86_64:
-- The PIC base register points to the PIC base label at the beginning
-- of the current CmmDecl. We just have to use a label difference to
-- get the offset.
-- We have already made sure that all labels that are not from the current
-- module are accessed indirectly ('as' can't calculate differences between
-- undefined labels).
picRelative width arch OSDarwin lbl
        | arch /= ArchX86_64
        = CmmLabelDiffOff lbl mkPicBaseLabel 0 width

-- On AIX we use an indirect local TOC anchored by 'gotLabel'.
-- This way we use up only one global TOC entry per compilation-unit
-- (this is quite similar to GCC's @-mminimal-toc@ compilation mode)
picRelative width _ OSAIX lbl
        = CmmLabelDiffOff lbl gotLabel 0 width

-- PowerPC Linux:
-- The PIC base register points to our fake GOT. Use a label difference
-- to get the offset.
-- We have made sure that *everything* is accessed indirectly, so this
-- is only used for offsets from the GOT to symbol pointers inside the
-- GOT.
picRelative width ArchPPC os lbl
        | osElfTarget os
        = CmmLabelDiffOff lbl gotLabel 0 width


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
        = panic "GHC.CmmToAsm.PIC.picRelative undefined for this platform"



--------------------------------------------------------------------------------

needImportedSymbols :: NCGConfig -> Bool
needImportedSymbols config
        | os    == OSDarwin
        , arch  /= ArchX86_64
        = True

        | os    == OSAIX
        = True

        -- PowerPC Linux: -fPIC or -dynamic
        | osElfTarget os
        , arch  == ArchPPC
        = ncgPIC config || ncgExternalDynamicRefs config

        -- PowerPC 64 Linux: always
        | osElfTarget os
        , arch == ArchPPC_64 ELF_V1 || arch == ArchPPC_64 ELF_V2
        = True

        -- i386 (and others?): -dynamic but not -fPIC
        | osElfTarget os
        , arch /= ArchPPC_64 ELF_V1 && arch /= ArchPPC_64 ELF_V2
        = ncgExternalDynamicRefs config &&
          not (ncgPIC config)

        | otherwise
        = False
   where
      platform = ncgPlatform config
      arch     = platformArch platform
      os       = platformOS   platform

-- gotLabel
-- The label used to refer to our "fake GOT" from
-- position-independent code.
gotLabel :: CLabel
gotLabel
        -- HACK: this label isn't really foreign
        = mkForeignLabel
                (fsLit ".LCTOC1")
                ForeignLabelInThisPackage IsData



-- Emit GOT declaration
-- Output whatever needs to be output once per .s file.
--
-- We don't need to declare any offset tables.
-- However, for PIC on x86, we need a small helper function.
pprGotDeclaration :: NCGConfig -> HDoc
pprGotDeclaration config = case (arch,os) of
   (_, OSDarwin) -> empty

   -- Emit XCOFF TOC section
   (_, OSAIX)
        -> lines_ [ text ".toc"
                  , text ".tc ghc_toc_table[TC],.LCTOC1"
                  , text ".csect ghc_toc_table[RW]"
                    -- See Note [.LCTOC1 in PPC PIC code]
                  , text ".set .LCTOC1,$+0x8000"
                  ]


   -- PPC 64 ELF v1 needs a Table Of Contents (TOC)
   (ArchPPC_64 ELF_V1, _)
        -> line $ text ".section \".toc\",\"aw\""

   -- In ELF v2 we also need to tell the assembler that we want ABI
   -- version 2. This would normally be done at the top of the file
   -- right after a file directive, but I could not figure out how
   -- to do that.
   (ArchPPC_64 ELF_V2, _)
        -> lines_ [ text ".abiversion 2",
                    text ".section \".toc\",\"aw\""
                  ]

   (arch, os)
        | osElfTarget os
        , arch /= ArchPPC_64 ELF_V1 && arch /= ArchPPC_64 ELF_V2
        , not (ncgPIC config)
        -> empty

        | osElfTarget os
        , arch /= ArchPPC_64 ELF_V1 && arch /= ArchPPC_64 ELF_V2
        -> lines_ [
                -- See Note [.LCTOC1 in PPC PIC code]
                text ".section \".got2\",\"aw\"",
                text ".LCTOC1 = .+32768" ]

   _ -> panic "pprGotDeclaration: no match"
 where
   platform = ncgPlatform config
   arch     = platformArch platform
   os       = platformOS   platform


--------------------------------------------------------------------------------
-- On Darwin, we have to generate our own stub code for lazy binding..
-- For each processor architecture, there are two versions, one for PIC
-- and one for non-PIC.
--

pprImportedSymbol :: NCGConfig -> CLabel -> HDoc
pprImportedSymbol config importedLbl = case (arch,os) of
   (ArchAArch64, OSDarwin)
        -> empty



   -- XCOFF / AIX
   --
   -- Similar to PPC64 ELF v1, there's dedicated TOC register (r2). To
   -- workaround the limitation of a global TOC we use an indirect TOC
   -- with the label `ghc_toc_table`.
   --
   -- See also GCC's `-mminimal-toc` compilation mode or
   -- http://www.ibm.com/developerworks/rational/library/overview-toc-aix/
   --
   -- NB: No DSO-support yet

   (_, OSAIX) -> case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> lines_ [
                   text "LC.." <> ppr_lbl lbl <> char ':',
                   text "\t.long" <+> ppr_lbl lbl ]
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

   (ArchPPC_64 _, _)
        | osElfTarget os
        -> case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> lines_ [
                   text ".LC_" <> ppr_lbl lbl <> char ':',
                   text "\t.quad" <+> ppr_lbl lbl ]
            _ -> empty

   _ | osElfTarget os
     -> case dynamicLinkerLabelInfo importedLbl of
            Just (SymbolPtr, lbl)
              -> let symbolSize = case ncgWordWidth config of
                         W32 -> text "\t.long"
                         W64 -> text "\t.quad"
                         _ -> panic "Unknown wordRep in pprImportedSymbol"

                 in lines_ [
                      text ".section \".got2\", \"aw\"",
                      text ".LC_" <> ppr_lbl lbl <> char ':',
                      symbolSize <+> ppr_lbl lbl ]

            -- PLT code stubs are generated automatically by the dynamic linker.
            _ -> empty

   _ -> panic "PIC.pprImportedSymbol: no match"
 where
   platform = ncgPlatform config
   ppr_lbl :: CLabel -> HLine
   ppr_lbl  = pprAsmLabel   platform
   arch     = platformArch  platform
   os       = platformOS    platform

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
        -> [NatCmmDecl RawCmmStatics PPC.Instr]
        -> NatM [NatCmmDecl RawCmmStatics PPC.Instr]

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
        :: OS -> Reg
        -> [NatCmmDecl (Alignment, RawCmmStatics) X86.Instr]
        -> NatM [NatCmmDecl (Alignment, RawCmmStatics) X86.Instr]

initializePicBase_x86 os picReg
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

initializePicBase_x86 OSDarwin picReg
        (CmmProc info lab live (ListGraph (entry:blocks)) : statics)
        = return (CmmProc info lab live (ListGraph (block':blocks)) : statics)

    where BasicBlock bID insns = entry
          block' = BasicBlock bID (X86.FETCHPC picReg : insns)

initializePicBase_x86 _ _ _
        = panic "initializePicBase_x86: not needed"

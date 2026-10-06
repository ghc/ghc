module GHC.Driver.Config.Cmm
  ( initCmmConfig
  ) where

import GHC.Cmm.Config

import GHC.Driver.DynFlags
import GHC.Driver.Backend

import GHC.Platform

import GHC.Prelude

initCmmConfig :: DynFlags -> CmmConfig
initCmmConfig dflags = CmmConfig
  { cmmProfile             = targetProfile                dflags
  , cmmOptControlFlow      = gopt Opt_CmmControlFlow      dflags
  , cmmDoLinting           = gopt Opt_DoCmmLinting        dflags
  , cmmOptElimCommonBlks   = gopt Opt_CmmElimCommonBlocks dflags
  , cmmOptSink             = gopt Opt_CmmSink             dflags
  , cmmOptThreadSanitizer  = gopt Opt_CmmThreadSanitizer dflags
  , cmmGenStackUnwindInstr = debugLevel dflags > 0
  , cmmExternalDynamicRefs = gopt Opt_ExternalDynamicRefs dflags
  , cmmDoCmmSwitchPlans    = not (backendHasNativeSwitch (backend dflags))
  , cmmSplitProcPoints     = not (backendSupportsUnsplitProcPoints (backend dflags))
                             || not (platformTablesNextToCode platform
                                     || ncgUnsplitWithoutTNTC (platformArch platform))
  }
  where platform                = targetPlatform dflags

-- | Can the native code generator for this architecture keep proc points
-- unsplit without tables-next-to-code? The info tables of continuations are
-- then emitted out of line; see Note [Proc points without
-- tables-next-to-code] in "GHC.Cmm.Info". With tables-next-to-code, the
-- native code generator never needs to split proc points.
ncgUnsplitWithoutTNTC :: Arch -> Bool
ncgUnsplitWithoutTNTC arch = case arch of
  -- Each proc becomes one wasm function, and a wasm function cannot be
  -- entered at one of its blocks, so every proc point must be a proc.
  ArchWasm32   -> False
  -- The TOC pointer (r2) is set up only by the prologue at a proc's entry
  -- label (and with ELF v1 the entry label is a function descriptor), so a
  -- continuation entered from other code would run with a wrong TOC.
  ArchPPC_64 _ -> False
  -- x86, x86_64, AArch64, PPC, RISCV64 and LoongArch64 enter every entry
  -- block of a proc correctly. With PIC on ELF, initializePicBase_x86 and
  -- initializePicBase_ppc set up the PIC base at every entry block. i386
  -- with PIC on Darwin sets it up at the proc's entry only and would need
  -- split proc points (Note [inconsistent-pic-reg] in GHC.Cmm.Pipeline);
  -- the condition with tables-next-to-code does not exclude it either, and
  -- i386-darwin is no longer supported.
  _            -> True

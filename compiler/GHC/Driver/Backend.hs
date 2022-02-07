{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- {-# OPTIONS_GHC -Werror #-}

module GHC.Driver.Backend
   ( ncgBackend
   , prototypeBackend
   , llvmBackend
   , viaCBackend
   , interpreterBackend
   , noBackend
   , platformDefaultBackend
   , platformNcgSupported
   , module GHC.Driver.Backend.Types
   )

where

-- | What constitutes a back end for code generation

import GHC.Prelude

import GHC.Driver.Backend.Types
import GHC.Driver.Phases


import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Driver.Pipeline.Monad
import GHC.Platform


platformDefaultBackend :: Platform -> Backend
platformDefaultBackend platform = if
      | platformUnregisterised platform -> viaCBackend
      | platformNcgSupported platform   -> ncgBackend
      | otherwise                       -> llvmBackend

-- | Is the platform supported by the Native Code Generator?
platformNcgSupported :: Platform -> Bool
platformNcgSupported platform = if
      | platformUnregisterised platform -> False -- NCG doesn't support unregisterised ABI
      | ncgValidArch                    -> True
      | otherwise                       -> False
   where
      ncgValidArch = case platformArch platform of
         ArchX86       -> True
         ArchX86_64    -> True
         ArchPPC       -> True
         ArchPPC_64 {} -> True
         ArchAArch64   -> True
         _             -> False




prototypeBackend, ncgBackend, llvmBackend, viaCBackend, interpreterBackend, noBackend
    :: Backend

-- | The prototype back end is meant to be used as a starting point for
-- defining real back ends.  Where a field has a value that is common to
-- most back ends, the prototype is initialized with that field.  (If
-- values are split roughly evenly, "most" is awarded to "real" back ends
-- that generate some kind of code, not to the interpreter or to the back
-- end of `-fno-code`.)  Where there is no common value, the field is
-- initialized with a closure that panics on evaluation.
--
-- For the actual field values, so you know which fields to update,
-- consult the source.

prototypeBackend =
    Backend { backendDescription = missing "Description"
            , backendWritesFiles = True
            , backendPipelineOutput = Persistent
            , backendGeneratesCode = True
            , backendSupportsInterfaceWriting = True
            , backendRespectsSpecialise = True
            , backendWantsGlobalBindings = False
            , backendHasNativeSwitch = False
            , backendPrimitiveImplementation = GenericPrimitives
            , backendSimdValidity =
                NotValid $ unlines [ "SIMD vector instructions require the LLVM back-end."
                                   , "Please use -fllvm."]
            , backendSupportsEmbeddedBlobs = False
            , backendNeedsPlatformNcgSupport = False
            , backendSupportsUnsplitProcPoints = False

            , backendSwappableWithViaC = False
            , backendUnregisterisedAbiOnly = False
            , backendGeneratesHc = False

            , backendSptIsDynamic = False
            , backendWantsBreakpointTicks = False
            , backendForcesOptimization0 = False
            , backendNeedsFullWays = False
            , backendSpecialModuleSource = const Nothing

            , backendSupportsHpc = True
            , backendValidityOfCImport = IsValid
            , backendValidityOfCExport = IsValid

            ----------------- supporting tooling
            -- | The assembler used on the code that is written by this back end.
            -- A program determined by a combination of back end,
            -- DynFlags, and Platform is run with the given `Option`s.
            , backendAssemblerProg = StandardAssemblerProg -- \logger dflags _platform -> runAs logger dflags
            , backendAssemblerInfoGetter = StandardAssemblerInfoGetter
                -- \logger dflags _platform -> getAssemblerInfo logger dflags

            , backendCDefs = NoCDefs -- \_ _ -> return []


            ----------------- code generation and compiler driver

            , backendCodeOutput = missing "CodeOutput"
            , backendPostHscPipeline = NoPostHscPipeline
                 -- \ _ _ _ _ -> return Nothing

            , backendNormalSuccessorPhase = missing "NormalSuccessorPhase"
            }
  where missing fname =
            panic $ "in a back end, field `backend" ++ fname ++ "` was not initialized"






-- | Native code generator backend.
--
-- Compiles Cmm code into textual assembler, then relies on
-- an external assembler toolchain to produce machine code.
--
-- Only supports a few platforms (X86, PowerPC, SPARC).
--
-- See "GHC.CmmToAsm".

ncgBackend =
    prototypeBackend { backendDescription = "native code generator"
                     , backendPrimitiveImplementation = NcgPrimitives
                     , backendSupportsEmbeddedBlobs = True
                     , backendNeedsPlatformNcgSupport = True
                     , backendSupportsUnsplitProcPoints = True
                     , backendSwappableWithViaC = True

                     , backendCodeOutput = NcgCodeOutput -- outputAsm
                     , backendPostHscPipeline = NcgPostHscPipeline -- asPipeline False
                     , backendNormalSuccessorPhase = As False
                     }



-- | LLVM backend.
--
-- Compiles Cmm code into LLVM textual IR, then relies on
-- LLVM toolchain to produce machine code.
--
-- It relies on LLVM support for the calling convention used
-- by the NCG backend to produce code objects ABI compatible
-- with it (see "cc 10" or "ghccc" calling convention in
-- https://llvm.org/docs/LangRef.html#calling-conventions).
--
-- Support a few platforms (X86, AArch64, s390x, ARM).
--
-- See "GHC.CmmToLlvm"

llvmBackend =
    prototypeBackend { backendDescription = "LLVM"
                     , backendHasNativeSwitch = True
                     , backendPrimitiveImplementation = LlvmPrimitives
                     , backendSimdValidity = IsValid
                     , backendSwappableWithViaC = True

        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)

                     , backendAssemblerProg = DarwinClangAssemblerProg
                          {-
                         \logger dflags platform ->
                             if platformOS platform == OSDarwin then
                               runClang logger dflags
                             else
                               runAs logger dflags
                          -}
                     , backendAssemblerInfoGetter = DarwinClangAssemblerInfoGetter
{-
                         \logger dflags platform ->
                             if platformOS platform == OSDarwin then
                               pure Clang
                             else
                               getAssemblerInfo logger dflags
-}
                     , backendCDefs = LlvmCDefs -- llvmCDefs
                     , backendCodeOutput = LlvmCodeOutput -- outputLlvm
                     , backendPostHscPipeline = LlvmPostHscPipeline -- llvmPipeline


                     , backendNormalSuccessorPhase = LlvmOpt

                     }

-- | Via-C backend.
--
-- Compiles Cmm code into C code, then relies on a C compiler
-- to produce machine code.
--
-- It produces code objects that are *not* ABI compatible
-- with those produced by NCG and LLVM backends.
--
-- Produced code is expected to be less efficient than the
-- one produced by NCG and LLVM backends because STG
-- registers are not pinned into real registers.  On the
-- other hand, it supports more target platforms (those
-- having a valid C toolchain).
--
-- See "GHC.CmmToC"

viaCBackend =
    prototypeBackend { backendDescription = "compiling via C"

                     , backendUnregisterisedAbiOnly = True
                     , backendGeneratesHc = True
                     , backendHasNativeSwitch = True

                     , backendCodeOutput = ViaCCodeOutput -- outputC
                     , backendPostHscPipeline = ViaCPostHscPipeline -- viaCPipeline HCc
                     , backendNormalSuccessorPhase = HCc
                     }


-- | No code generated (implements -fno-code).
--
-- Use this to disable code generation. It is particularly
-- useful when GHC is used as a library for other purpose
-- than generating code (e.g. to generate documentation with
-- Haddock) or when the user requested it (via -fno-code) for
-- some reason.

noBackend =
    prototypeBackend { backendDescription = "no code generated"
                     , backendWritesFiles = False
                     , backendPipelineOutput = NoOutputFile
                     , backendGeneratesCode = False
                     , backendSupportsInterfaceWriting = False
                     , backendRespectsSpecialise = False
                     , backendWantsGlobalBindings = True
                     , backendSpecialModuleSource = const (Just "nothing")
                     , backendCodeOutput = panic "codeOutput: noBackend"
                     , backendNormalSuccessorPhase = StopLn
                     }


-- | ByteCode interpreter.
--
-- Produce ByteCode objects (BCO, see "GHC.ByteCode") that
-- can be interpreted. It is used by GHCi.
--
-- Currently some extensions are not supported
-- (foreign primops).
--
-- See "GHC.StgToByteCode"

interpreterBackend = -- implements -fno-code
    prototypeBackend { backendDescription = "byte-code interpreter"
                     , backendWritesFiles = False
                     , backendPipelineOutput = NoOutputFile
                     , backendRespectsSpecialise = False
                     , backendWantsGlobalBindings = True
                     , backendSptIsDynamic = True
                     , backendWantsBreakpointTicks = True
                     , backendForcesOptimization0 = True
                     , backendNeedsFullWays = True
                     , backendSpecialModuleSource =
                         \recomp -> if recomp then Just "interpreted" else Nothing
                     , backendSupportsHpc = False
                     , backendValidityOfCExport =
                         NotValid (text $ "requires unregisterised, llvm (-fllvm) or " ++
                                          "native code generation (-fasm)")

                     , backendCodeOutput = panic "codeOutput: interpreterBackend"
                     , backendNormalSuccessorPhase = StopLn
                     }


{-# LANGUAGE MultiWayIf #-}

-- | Code generation backends
module GHC.Driver.Backend
   ( Backend (..)
   , platformDefaultBackend
   , platformNcgSupported
   , backendProducesObject
   , backendRetainsAllBindings
   )
where

import GHC.Prelude
import GHC.Platform

-- | Code generation backends.
--
-- GHC supports several code generation backends serving different purposes
-- (producing machine code, producing ByteCode for the interpreter) and
-- supporting different platforms.
--
data Backend
   = NCG           -- ^ Native code generator backend.
                   --
                   -- Compiles Cmm code into textual assembler, then relies on
                   -- an external assembler toolchain to produce machine code.
                   --
                   -- Only supports a few platforms (X86, PowerPC, SPARC).
                   --
                   -- See "GHC.CmmToAsm".


   | LLVM          -- ^ LLVM backend.
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


   | ViaC          -- ^ Via-C backend.
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


   | Interpreter   -- ^ ByteCode interpreter.
                   --
                   -- Produce ByteCode objects (BCO, see "GHC.ByteCode") that
                   -- can be interpreted. It is used by GHCi.
                   --
                   -- Currently some extensions are not supported
                   -- (foreign primops).
                   --
                   -- See "GHC.StgToByteCode"


   | NoBackend     -- ^ No code generated.
                   --
                   -- Use this to disable code generation. It is particularly
                   -- useful when GHC is used as a library for other purpose
                   -- than generating code (e.g. to generate documentation with
                   -- Haddock) or when the user requested it (via -fno-code) for
                   -- some reason.

   deriving (Eq,Ord,Show,Read)

-- | Default backend to use for the given platform.
platformDefaultBackend :: Platform -> Backend
platformDefaultBackend platform = if
      | platformUnregisterised platform -> ViaC
      | platformNcgSupported platform   -> NCG
      | otherwise                       -> LLVM


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
         ArchSPARC     -> True
         ArchAArch64   -> True
         _             -> False

-- | Will this backend produce an object file on the disk?
backendProducesObject :: Backend -> Bool
backendProducesObject ViaC        = True
backendProducesObject NCG         = True
backendProducesObject LLVM        = True
backendProducesObject Interpreter = False
backendProducesObject NoBackend   = False

-- | Does this backend retain *all* top-level bindings for a module,
-- rather than just the exported bindings, in the TypeEnv and compiled
-- code (if any)?
--
-- Interpreter backend does this, so that GHCi can call functions inside a
-- module.
--
-- When no backend is used we also do it, so that Haddock can get access to the
-- GlobalRdrEnv for a module after typechecking it.
backendRetainsAllBindings :: Backend -> Bool
backendRetainsAllBindings Interpreter = True
backendRetainsAllBindings NoBackend   = True
backendRetainsAllBindings ViaC        = False
backendRetainsAllBindings NCG         = False
backendRetainsAllBindings LLVM        = False

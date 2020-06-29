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

-- | Backend
data Backend
   = NCG           -- ^ Native code generator backend
   | LLVM          -- ^ LLVM backend
   | ViaC          -- ^ Via-C backend
   | Interpreter   -- ^ Interpreter
   | NoBackend     -- ^ No code generated (used by Haddock, -fno-code, ghc-api, etc.)
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

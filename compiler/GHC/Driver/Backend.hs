{-# LANGUAGE MultiWayIf #-}

-- | Code generation backends
module GHC.Driver.Backend
   ( Backend (..)
   , platformDefaultBackend
   , platformNcgSupported
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

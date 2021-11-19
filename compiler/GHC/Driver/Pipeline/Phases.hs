{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Driver.Pipeline.Phases (TPhase(..), PhaseHook(..)) where

import GHC.Prelude
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Env.Types
import GHC.Driver.Session
import GHC.Driver.CmdLine
import GHC.Types.SourceFile
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Status
import GHC.Tc.Types ( FrontendResult )
import GHC.Types.Error
import GHC.Driver.Errors.Types
import GHC.Fingerprint.Type
import GHC.Unit.Module.Location ( ModLocation )
import GHC.Unit.Module.Name ( ModuleName )
import GHC.Unit.Module.ModIface
import GHC.Linker.Types
import GHC.Driver.Phases

-- Typed Pipeline Phases
-- MP: TODO: We need to refine the arguments to each of these phases so recompilation
-- can be smarter. For example, rather than passing a whole HscEnv, just pass the options
-- which each phase depends on, then recompilation checking can decide to only rerun each
-- phase if the inputs have been modified.
data TPhase res where
  T_Unlit :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_FileArgs :: HscEnv -> FilePath -> TPhase (DynFlags, [Warn])
  T_Cpp   :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_HsPp  :: PipeEnv -> HscEnv -> FilePath -> FilePath -> TPhase FilePath
  T_HscRecomp :: PipeEnv -> HscEnv -> FilePath -> HscSource -> TPhase (HscEnv, ModSummary, HscRecompStatus)
  T_Hsc :: HscEnv -> ModSummary -> TPhase (FrontendResult, Messages GhcMessage)
  T_HscPostTc :: HscEnv -> ModSummary
              -> FrontendResult
              -> Messages GhcMessage
              -> Maybe Fingerprint
              -> TPhase HscBackendAction
  T_HscBackend :: PipeEnv -> HscEnv -> ModuleName -> HscSource -> ModLocation -> HscBackendAction -> TPhase ([FilePath], ModIface, Maybe Linkable, FilePath)
  T_CmmCpp :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_Cmm :: PipeEnv -> HscEnv -> FilePath -> TPhase ([FilePath], FilePath)
  T_Cc :: Phase -> PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_As :: Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> TPhase FilePath
  T_LlvmOpt :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_LlvmLlc :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_LlvmMangle :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_MergeForeign :: PipeEnv -> HscEnv -> FilePath -> [FilePath] -> TPhase FilePath

-- | A wrapper around the interpretation function for phases.
data PhaseHook = PhaseHook (forall a . TPhase a -> IO a)

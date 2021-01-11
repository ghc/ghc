{-# LANGUAGE GADTs #-}

module GHC.Driver.Errors.Types (
    GhcMessage(..)
  , DriverMessage(..)
  -- * Constructors
  , ghcUnknownMessage
  , mkDriverWarn
  ) where

import Data.Typeable

import GHC.Core.InstEnv ( ClsInst )
import GHC.Driver.Env.Types
import GHC.Driver.Flags
import GHC.Driver.Session ( DynFlags )
import GHC.Prelude ( String )
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Unit.Finder.Types ( FindResult )
import GHC.Unit.Module.Name ( ModuleName )
import GHC.Unit.State
import GHC.Unit.Types ( UnitId, Module )
import GHC.Utils.Outputable

import GHC.Parser.Errors.Types ( PsMessage )
import GHC.Tc.Errors.Types ( TcRnMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )

-- | The umbrella type that encompasses all the different messages that GHC might output during the
-- different compilation stages.
data GhcMessage where
  -- | A message from the parsing phase.
  GhcPsMessage      :: PsMessage -> GhcMessage
  -- | A message from typecheck/renaming phase.
  GhcTcRnMessage    :: TcRnMessage -> GhcMessage
  -- | A message from the desugaring (HsToCore) phase.
  GhcDsMessage      :: DsMessage -> GhcMessage
  -- | A message from the driver.
  GhcDriverMessage  :: DriverMessage -> GhcMessage
  -- | An \"escape\" hatch which can be used when we don't know the source of the message or
  -- if the message is not one of the typed ones. The 'RenderableDiagnostic' and 'Typeable' constraints
  -- ensure that if we /know/, at pattern-matching time, the originating type, we can attempt a cast and
  -- access the fully-structured error. This would be the case for a GHC plugin that offers a domain-specific
  -- error type but that doesn't want to place the burden on IDEs/application code to \"know\" it.
  -- The 'RenderableDiagnostic' constraints ensures that worst case scenario we can still render this
  -- into something which can be eventually converted into an 'SDoc'.
  GhcUnknownMessage :: forall a. (RenderableDiagnostic a, Typeable a) => a -> GhcMessage

ghcUnknownMessage :: DecoratedSDoc -> GhcMessage
ghcUnknownMessage = GhcUnknownMessage

type Reasons = Messages TcRnMessage

-- | A message from the driver.
data DriverMessage
  = -- Warnings
    DriverWarnModuleInferredUnsafe !DynFlags !ModuleName [ClsInst] Reasons
  | DriverWarnInferredSafeImports  !ModuleName

    -- Errors
  | DriverCannotFindModule !HscEnv !ModuleName !FindResult
  | DriverNotAnExpression !String
  | DriverParseErrorImport
  | DriverPkgRequiredTrusted !UnitState !UnitId
  | DriverCantLoadIfaceForSafe !Module
  | DriverUnknownMessage !DecoratedSDoc

-- | Construct an structured error out of the input driver message.
mkDriverWarn :: WarnReason -> SrcSpan -> PrintUnqualified -> DriverMessage -> MsgEnvelope DriverMessage
mkDriverWarn reason loc qual warn =
  makeIntoWarning reason (mkErr loc qual warn)

{-# LANGUAGE GADTs #-}

module GHC.Driver.Errors.Types (
    GhcMessage(..)
  , DriverMessage(..)
  -- * Constructors
  , ghcUnknownMessage
  ) where

import Data.Typeable

import GHC.Core.InstEnv ( ClsInst )
import GHC.Driver.Env.Types
import GHC.Driver.Session ( DynFlags )
import GHC.Prelude ( String )
import GHC.Types.Error
import GHC.Unit.Finder.Types ( FindResult )
import GHC.Unit.Module.Name ( ModuleName )
import GHC.Unit.State
import GHC.Unit.Types ( UnitId, Module )

import GHC.Parser.Errors.Types ( PsMessage )
import GHC.Tc.Errors.Types ( TcRnMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )

{- Note [GhcMessage]
~~~~~~~~~~~~~~~~~~~~

Things can go wrong within GHC, and we might need to report diagnostics (error and/or warnings) to the
users. The 'GhcMessage' type is the root of the diagnostic hierarchy.

It's useful to have a separate type constructor for the different stages of the compilation pipeline.
This is not just helpful for tools, as it gives a clear indication on where the error occurred exactly,
but it's also necessary to allow 'handleSourceError' to be able to catch the relevant exception. In
particular, it allows the user to write something like:

handleMyErrors = handleSourceError (map handleInvididualError $ getMessages srcErrorMessages )
  where
    handleInvididualError e = case errMsgDiagnostic of
      GhcPsMessage _   -> .. -- error arose during parsing;
      GhcTcRnMessage _ -> .. -- error arose during TcRn
      ...

-}

-- | The umbrella type that encompasses all the different messages that GHC might output during the
-- different compilation stages. See Note [GhcMessage].
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
  -- if the message is not one of the typed ones. The 'Diagnostic' and 'Typeable' constraints
  -- ensure that if we /know/, at pattern-matching time, the originating type, we can attempt a cast and
  -- access the fully-structured error. This would be the case for a GHC plugin that offers a domain-specific
  -- error type but that doesn't want to place the burden on IDEs/application code to \"know\" it.
  -- The 'Diagnostic' constraints ensures that worst case scenario we can still render this
  -- into something which can be eventually converted into an 'SDoc'.
  GhcUnknownMessage :: forall a. (Diagnostic a, Typeable a) => a -> GhcMessage

-- | Creates a new 'GhcMessage' out of a 'DiagnosticMessage'. This function is provided to ease the integration
-- of #18516 (structured-error-messages) by allowing unstructured errors to be wrapped into the general
-- (but structured) 'GhcMessage' type, so that the conversion can happen gradually. Ideally, this function
-- should be needed very rarely within GHC, as it would typically be used by plugin or library authors
-- (see comment for the 'GhcUnknownMessage' type constructor).
ghcUnknownMessage :: (Diagnostic a, Typeable a) => a -> GhcMessage
ghcUnknownMessage = GhcUnknownMessage

-- | A message from the driver.
data DriverMessage
  = -- Warnings
    DriverWarnModuleInferredUnsafe !DynFlags !ModuleName [ClsInst] !(Messages TcRnMessage)
  | DriverWarnInferredSafeImports  !ModuleName

    -- Errors
  | DriverCannotFindModule !HscEnv !ModuleName !FindResult
  | DriverNotAnExpression !String
  | DriverParseErrorImport
  | DriverPkgRequiredTrusted !UnitState !UnitId
  | DriverCantLoadIfaceForSafe !Module
  | DriverUnknownMessage !DecoratedSDoc

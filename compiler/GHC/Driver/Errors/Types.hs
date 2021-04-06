{-# LANGUAGE GADTs #-}

module GHC.Driver.Errors.Types (
    GhcMessage(..)
  , DriverMessage(..)
  , WarningMessages
  , ErrorMessages
  , WarnMsg
  -- * Constructors
  , ghcUnknownMessage
  -- * Utility functions
  , hoistMessageBiM
  , hoistTcRnMessage
  , hoistTcRnDsMessage
  , tcRnDsToGhcMessage
  , foldMessages
  ) where

import GHC.Prelude

import Data.Typeable
import GHC.Types.Error

import GHC.Parser.Errors.Types ( PsMessage )
import GHC.Tc.Errors.Types ( TcRnDsMessage(..), TcRnMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )
import Data.Bifunctor

type WarningMessages = Messages GhcMessage
type ErrorMessages   = Messages GhcMessage
type WarnMsg         = MsgEnvelope GhcMessage


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
      GhcPsMessage _   -> .. -- diagnostic emitted during parsing;
      GhcTcRnMessage _ -> .. -- diagnostic emitted during TcRn
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
  -- The 'Diagnostic' constraint ensures that worst case scenario we can still render this
  -- into something which can be eventually converted into a 'DecoratedSDoc'.
  GhcUnknownMessage :: forall a. (Diagnostic a, Typeable a) => a -> GhcMessage

-- | Creates a new 'GhcMessage' out of any diagnostic. This function is also provided to ease the integration
-- of #18516 by allowing diagnostics to be wrapped into the general (but structured) 'GhcMessage' type,
-- so that the conversion can happen gradually. This function should be needed very rarely within GHC,
-- as it would typically be used by plugin or library authors (see comment for the 'GhcUnknownMessage'
-- type constructor)
ghcUnknownMessage :: (Diagnostic a, Typeable a) => a -> GhcMessage
ghcUnknownMessage = GhcUnknownMessage

-- | Hoist a transformation into a 'Bifunctor' wrapped in a 'Monad'. Abstracts away the classic pattern
-- where we are calling 'ioMsgMaybe' on the result of 'IO (Messages e, a)'.
hoistMessageBiM :: (Monad m, Bifunctor f)
                => (e -> GhcMessage)
                -- ^ A function to transform 'e' into a 'GhcMessage'.
                -> m (f (Messages e) a)
                -- ^ A 'Bifunctor' @f@ wrapped into a 'Monad' @m@.
                -> m (f (Messages GhcMessage) a)
hoistMessageBiM f m = fmap (first (fmap f)) m

-- | Given a collection of @e@ wrapped in a 'Foldable' structure, converts it into 'Messages'
-- via the supplied transformation function.
foldMessages :: Foldable f
             => (e -> MsgEnvelope GhcMessage)
             -> f e
             -> Messages GhcMessage
foldMessages f = foldl' (\acc m -> addMessage (f m) acc) emptyMessages

-- | Abstracts away the classic pattern where we are calling 'ioMsgMaybe' on the result of
-- 'IO (Messages TcRnMessage, a)'.
hoistTcRnMessage :: Monad m => m (Messages TcRnMessage, a) -> m (Messages GhcMessage, a)
hoistTcRnMessage = hoistMessageBiM GhcTcRnMessage

-- | Abstracts away the classic pattern where we are calling 'ioMsgMaybe' on the result of
-- 'IO (Messages TcRnDsMessage, a)'.
hoistTcRnDsMessage :: Monad m => m (Messages TcRnDsMessage, a) -> m (Messages GhcMessage, a)
hoistTcRnDsMessage = hoistMessageBiM tcRnDsToGhcMessage

-- | Converts the input 'TcRnDsMessage' into a 'GhcMessage'.
tcRnDsToGhcMessage :: TcRnDsMessage -> GhcMessage
tcRnDsToGhcMessage (TcRnDsMessage m) = either GhcDsMessage GhcTcRnMessage m

-- | A message from the driver.
data DriverMessage
  = DriverUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- instances will be added in the future (#18516).

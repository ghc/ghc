{-# LANGUAGE GADTs #-}

module GHC.Driver.Errors.Types (
    GhcMessage(..)
  , DriverMessage(..), DriverMessages
  , WarningMessages
  , ErrorMessages
  , WarnMsg
  -- * Constructors
  , ghcUnknownMessage
  -- * Utility functions
  , hoistTcRnMessage
  , hoistDsMessage
  , foldPsMessages
  ) where

import GHC.Prelude

import Data.Typeable
import GHC.Types.Error

import GHC.Parser.Errors       ( PsErrorDesc, PsHint )
import GHC.Parser.Errors.Types ( PsMessage )
import GHC.Tc.Errors.Types     ( TcRnMessage )
import GHC.HsToCore.Errors.Types ( DsMessage )
import Data.Bifunctor

-- | A collection of warning messages.
-- /INVARIANT/: Each 'GhcMessage' in the collection should have 'SevWarning' severity.
type WarningMessages = Messages GhcMessage

-- | A collection of error messages.
-- /INVARIANT/: Each 'GhcMessage' in the collection should have 'SevError' severity.
type ErrorMessages   = Messages GhcMessage

-- | A single warning message.
-- /INVARIANT/: It must have 'SevWarning' severity.
type WarnMsg         = MsgEnvelope GhcMessage


{- Note [GhcMessage]
~~~~~~~~~~~~~~~~~~~~

We might need to report diagnostics (error and/or warnings) to the users. The
'GhcMessage' type is the root of the diagnostic hierarchy.

It's useful to have a separate type constructor for the different stages of
the compilation pipeline. This is not just helpful for tools, as it gives a
clear indication on where the error occurred exactly. Furthermore it increases
the modularity amongst the different components of GHC (i.e. to avoid having
"everything depend on everything else") and allows us to write separate
functions that renders the different kind of messages.

-}

-- | The umbrella type that encompasses all the different messages that GHC
-- might output during the different compilation stages. See
-- Note [GhcMessage].
data GhcMessage where
  -- | A message from the parsing phase.
  GhcPsMessage      :: PsMessage -> GhcMessage
  -- | A message from typecheck/renaming phase.
  GhcTcRnMessage    :: TcRnMessage -> GhcMessage
  -- | A message from the desugaring (HsToCore) phase.
  GhcDsMessage      :: DsMessage -> GhcMessage
  -- | A message from the driver.
  GhcDriverMessage  :: DriverMessage -> GhcMessage

  -- | An \"escape\" hatch which can be used when we don't know the source of
  -- the message or if the message is not one of the typed ones. The
  -- 'Diagnostic' and 'Typeable' constraints ensure that if we /know/, at
  -- pattern-matching time, the originating type, we can attempt a cast and
  -- access the fully-structured error. This would be the case for a GHC
  -- plugin that offers a domain-specific error type but that doesn't want to
  -- place the burden on IDEs/application code to \"know\" it. The
  -- 'Diagnostic' constraint ensures that worst case scenario we can still
  -- render this into something which can be eventually converted into a
  -- 'DecoratedSDoc'.
  GhcUnknownMessage :: forall a. (Diagnostic a, Typeable a) => a -> GhcMessage

-- | Creates a new 'GhcMessage' out of any diagnostic. This function is also
-- provided to ease the integration of #18516 by allowing diagnostics to be
-- wrapped into the general (but structured) 'GhcMessage' type, so that the
-- conversion can happen gradually. This function should not be needed within
-- GHC, as it would typically be used by plugin or library authors (see
-- comment for the 'GhcUnknownMessage' type constructor)
ghcUnknownMessage :: (Diagnostic a, Typeable a) => a -> GhcMessage
ghcUnknownMessage = GhcUnknownMessage

-- | Given a collection of @e@ wrapped in a 'Foldable' structure, converts it
-- into 'Messages' via the supplied transformation function.
foldPsMessages :: Foldable f
               => (e -> MsgEnvelope PsMessage)
               -> f e
               -> Messages GhcMessage
foldPsMessages f = foldMap (singleMessage . fmap GhcPsMessage . f)

-- | Abstracts away the frequent pattern where we are calling 'ioMsgMaybe' on
-- the result of 'IO (Messages TcRnMessage, a)'.
hoistTcRnMessage :: Monad m => m (Messages TcRnMessage, a) -> m (Messages GhcMessage, a)
hoistTcRnMessage = fmap (first (fmap GhcTcRnMessage))

-- | Abstracts away the frequent pattern where we are calling 'ioMsgMaybe' on
-- the result of 'IO (Messages DsMessage, a)'.
hoistDsMessage :: Monad m => m (Messages DsMessage, a) -> m (Messages GhcMessage, a)
hoistDsMessage = fmap (first (fmap GhcDsMessage))

-- | A message from the driver.
data DriverMessage
  = DriverUnknownMessage !DiagnosticMessage
  -- ^ Simply rewraps a generic 'DiagnosticMessage'. More
  -- constructors will be added in the future (#18516).
  | DriverPsHeaderMessage !PsErrorDesc ![PsHint]
  -- ^ A parse error in parsing a Haskell file header during dependency
  -- analysis

-- | A collection of driver messages
type DriverMessages = Messages DriverMessage

-- | A message about Safe Haskell.

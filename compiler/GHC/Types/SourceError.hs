-- | Source errors
module GHC.Types.SourceError
   ( SourceError (..)
   , mkSrcErr
   , srcErrorMessages
   , throwErrors
   , throwOneError
   , handleSourceError
   )
where

import GHC.Prelude
import GHC.Types.Error
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Exception
import GHC.Utils.Error (pprMsgEnvelopeBagWithLoc)
import GHC.Utils.Outputable

import GHC.Driver.Errors.Ppr () -- instance Diagnostic GhcMessage
import GHC.Driver.Errors.Types

import Control.Monad.Catch as MC (MonadCatch, catch)

mkSrcErr :: Messages GhcMessage -> SourceError
mkSrcErr = SourceError

srcErrorMessages :: SourceError -> Messages GhcMessage
srcErrorMessages (SourceError msgs) = msgs

throwErrors :: MonadIO io => Messages GhcMessage -> io a
throwErrors = liftIO . throwIO . mkSrcErr

throwOneError :: MonadIO io => MsgEnvelope GhcMessage -> io a
throwOneError = throwErrors . singleMessage

-- | A source error is an error that is caused by one or more errors in the
-- source code.  A 'SourceError' is thrown by many functions in the
-- compilation pipeline.  Inside GHC these errors are merely printed via
-- 'log_action', but API clients may treat them differently, for example,
-- insert them into a list box.  If you want the default behaviour, use the
-- idiom:
--
-- > handleSourceError printExceptionAndWarnings $ do
-- >   ... api calls that may fail ...
--
-- The 'SourceError's error messages can be accessed via 'srcErrorMessages'.
-- This list may be empty if the compiler failed due to @-Werror@
-- ('Opt_WarnIsError').
--
-- See 'printExceptionAndWarnings' for more information on what to take care
-- of when writing a custom error handler.
newtype SourceError = SourceError (Messages GhcMessage)

instance Show SourceError where
  -- We implement 'Show' because it's required by the 'Exception' instance, but diagnostics
  -- shouldn't be shown via the 'Show' typeclass, but rather rendered using the ppr functions.
  -- This also explains why there is no 'Show' instance for a 'MsgEnvelope'.
  show (SourceError msgs) =
      renderWithContext defaultSDocContext
    . vcat
    . pprMsgEnvelopeBagWithLoc
    . getMessages
    $ msgs

instance Exception SourceError

-- | Perform the given action and call the exception handler if the action
-- throws a 'SourceError'.  See 'SourceError' for more information.
handleSourceError :: (MonadCatch m) =>
                     (SourceError -> m a) -- ^ exception handler
                  -> m a -- ^ action to perform
                  -> m a
handleSourceError handler act =
  MC.catch act (\(e :: SourceError) -> handler e)


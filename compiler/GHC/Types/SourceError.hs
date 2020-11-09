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

import Data.Coerce (coerce)

import GHC.Prelude
import GHC.Data.Bag
import {-# SOURCE #-} GHC.Driver.Errors.Types (GhcError)
import GHC.Types.Error
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Exception

import Control.Monad.Catch as MC (MonadCatch, catch)

mkSrcErr :: ErrorMessages GhcError -> SourceError
mkSrcErr = SourceError

srcErrorMessages :: SourceError -> ErrorMessages GhcError
srcErrorMessages (SourceError msgs) = msgs

throwErrors :: MonadIO io => ErrorMessages GhcError -> io a
throwErrors = liftIO . throwIO . mkSrcErr . coerce

throwOneError :: MonadIO io => ErrMsg GhcError -> io a
throwOneError = throwErrors . ErrorMessages . unitBag

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
newtype SourceError = SourceError (ErrorMessages GhcError)

instance Show SourceError where
  show (SourceError (ErrorMessages msgs)) = unlines . map showErrMsg . bagToList $ msgs

instance Exception SourceError

-- | Perform the given action and call the exception handler if the action
-- throws a 'SourceError'.  See 'SourceError' for more information.
handleSourceError :: (MonadCatch m) =>
                     (SourceError -> m a) -- ^ exception handler
                  -> m a -- ^ action to perform
                  -> m a
handleSourceError handler act =
  MC.catch act (\(e :: SourceError) -> handler e)


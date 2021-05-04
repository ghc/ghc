{-# LANGUAGE LambdaCase #-}

module GHC.Types.DiagnosticReason where

import GHC.Prelude
import GHC.Driver.Flags
import GHC.Utils.Outputable

-- | The reason /why/ a 'Diagnostic' was emitted in the first place.
-- Diagnostic messages are born within GHC with a very precise reason, which
-- can be completely statically-computed (i.e. this is an error or a warning
-- no matter what), or influenced by the specific state of the 'DynFlags' at
-- the moment of the creation of a new 'Diagnostic'. For example, a parsing
-- error is /always/ going to be an error, whereas a 'WarningWithoutFlag
-- Opt_WarnUnusedImports' might turn into an error due to '-Werror' or
-- '-Werror=warn-unused-imports'. Interpreting a 'DiagnosticReason' together
-- with its associated 'Severity' gives us the full picture.
data DiagnosticReason
  = WarningWithoutFlag
  -- ^ Born as a warning.
  | WarningWithFlag !WarningFlag
  -- ^ Warning was enabled with the flag.
  | ErrorWithoutFlag
  -- ^ Born as an error.
  deriving (Eq, Show)

instance Outputable DiagnosticReason where
  ppr = \case
    WarningWithoutFlag  -> text "WarningWithoutFlag"
    WarningWithFlag wf  -> text ("WarningWithFlag " ++ show wf)
    ErrorWithoutFlag    -> text "ErrorWithoutFlag"

module GHC.Types.Error where

import GHC.Prelude (Maybe, Bool)
import GHC.Utils.Outputable (SDoc)
import GHC.Types.SrcLoc (SrcSpan)

data MessageClass
  = MCOutput
  | MCFatal
  | MCInteractive
  | MCDump
  | MCInfo
  | MCDiagnostic Severity ResolvedDiagnosticReason (Maybe DiagnosticCode) [SrcSpan]

data Severity
  = SevIgnore
  | SevWarning
  | SevError

data DiagnosticCode
data ResolvedDiagnosticReason

mkLocMessageWarningGroups  :: Bool -> MessageClass -> SrcSpan -> SDoc -> SDoc

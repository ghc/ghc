{-# LANGUAGE TypeFamilies #-}
module GHC.Types.Error
  ( MessageClass
  , Severity
  , DiagnosticCode
  , ResolvedDiagnosticReason
  , DiagnosticReason
  , mkLocMessageWarningGroups
  , getCaretDiagnostic
  , MsgEnvelope
  , NoDiagnosticOpts
  , UnknownDiagnosticFor
  , Diagnostic
  , DiagnosticOpts
  , DiagnosticHint
  , Messages
  , mkSimpleUnknownDiagnostic
  ) where

import GHC.Prelude
import GHC.Utils.Outputable (SDoc)
import GHC.Types.SrcLoc (SrcSpan)
import Data.Typeable (Typeable)
import Data.Kind (Type)

data MessageClass
data Severity

data DiagnosticCode
data ResolvedDiagnosticReason

mkLocMessageWarningGroups :: Bool -> MessageClass -> SrcSpan -> SDoc -> SDoc
getCaretDiagnostic :: MessageClass -> SrcSpan -> IO SDoc

-- Declared as standalone type families + abstract class to side step a bug with
-- oneshot mode and associated types in hs-boot files.
class Diagnostic (a :: Type)
type family DiagnosticOpts (a :: Type) :: Type
type family DiagnosticHint (a :: Type) :: Type

data UnknownDiagnostic (opts :: Type) (hint :: Type)
type UnknownDiagnosticFor a = UnknownDiagnostic (DiagnosticOpts a) (DiagnosticHint a)

data NoDiagnosticOpts

data Messages (e :: Type)
instance Functor Messages

data MsgEnvelope (e :: Type)

data DiagnosticReason

mkSimpleUnknownDiagnostic
  :: (Diagnostic a, Typeable a, DiagnosticOpts a ~ NoDiagnosticOpts)
  => a -> UnknownDiagnostic b (DiagnosticHint a)

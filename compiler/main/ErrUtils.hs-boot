module ErrUtils where

import GhcPrelude
import Outputable (PrintUnqualified )
import Outputable.DynFlags (SDoc)
import SrcLoc (SrcSpan)
import Json
import {-# SOURCE #-} DynFlags ( DynFlags, DumpFlag )

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive
  | SevDump
  | SevInfo
  | SevWarning
  | SevError


type MsgDoc = SDoc

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessageAnn :: Maybe String -> Severity -> SrcSpan -> MsgDoc -> MsgDoc
getCaretDiagnostic :: Severity -> SrcSpan -> IO MsgDoc
dumpSDoc :: DynFlags -> PrintUnqualified -> DumpFlag -> String -> SDoc -> IO ()

instance ToJson Severity

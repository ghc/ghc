{-# LANGUAGE RankNTypes #-}

module GHC.Utils.Error where

import GHC.Prelude
import GHC.Utils.Outputable (SDoc, PprStyle )
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Utils.Json
import {-# SOURCE #-} GHC.Driver.Session ( DynFlags )

type DumpAction = DynFlags -> PprStyle -> DumpOptions -> String
                  -> DumpFormat -> SDoc -> IO ()

type TraceAction = forall a. DynFlags -> String -> SDoc -> a -> a

data DumpOptions = DumpOptions
   { dumpForcedToFile :: Bool
   , dumpSuffix       :: String
   }

data DumpFormat
  = FormatHaskell
  | FormatCore
  | FormatSTG
  | FormatByteCode
  | FormatCMM
  | FormatASM
  | FormatC
  | FormatLLVM
  | FormatText

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
defaultDumpAction :: DumpAction
defaultTraceAction :: TraceAction

instance ToJson Severity

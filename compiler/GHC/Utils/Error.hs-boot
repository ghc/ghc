{-# LANGUAGE RankNTypes #-}

module GHC.Utils.Error where

import GHC.Prelude
import GHC.Utils.Outputable (SDoc, PprStyle )
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

defaultDumpAction :: DumpAction
defaultTraceAction :: TraceAction

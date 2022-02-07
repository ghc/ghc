{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}

module GHC.Driver.Backend.Pipeline
   ( -- hscPostBackendPipeline
--   , backendPipelineOutput
   )
where

import GHC.Prelude

import GHC.Platform

import GHC.Utils.Monad ( MonadIO(liftIO), mapMaybeM )

import GHC.Driver.Main
import GHC.Driver.Env hiding ( Hsc )
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Phases
import GHC.Driver.Pipeline.Execute
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Backend.Output
import GHC.Driver.Ppr
import GHC.Driver.Hooks

import GHC.Platform.Ways

import GHC.SysTools
import GHC.Utils.TmpFs

import GHC.Linker.ExtraObj
import GHC.Linker.Static
import GHC.Linker.Static.Utils
import GHC.Linker.Types

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Exception as Exception
import GHC.Utils.Logger

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString     ( mkFastString )
import GHC.Data.StringBuffer   ( hPutStringBuffer )
import GHC.Data.Maybe          ( expectJust )

import GHC.Iface.Make          ( mkFullIface )
import GHC.Runtime.Loader      ( initializePlugins )


import GHC.Types.Basic       ( SuccessFlag(..), ForeignSrcLang(..) )
import GHC.Types.Error       ( singleMessage, getMessages )
import GHC.Types.Target
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import GHC.Types.SourceError

import GHC.Unit
import GHC.Unit.Env
--import GHC.Unit.Finder
--import GHC.Unit.State
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph (needsTemplateHaskellOrQQ)
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo

import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import qualified Control.Monad.Catch as MC (handle)
import Data.Maybe
import Data.Either      ( partitionEithers )
import qualified Data.Set as Set

import Data.Time        ( getCurrentTime )
import GHC.Iface.Recomp

import GHC.Driver.Backend.Rep



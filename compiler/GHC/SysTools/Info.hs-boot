module GHC.SysTools.Info where

import GHC.Prelude

import {-# SOURCE #-} GHC.Driver.Session
import GHC.Utils.Logger

getAssemblerInfo :: Logger -> DynFlags -> IO CompilerInfo

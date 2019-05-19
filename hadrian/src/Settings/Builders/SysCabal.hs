module Settings.Builders.SysCabal (sysCabalBuilderArgs) where

import Settings.Builders.Common

sysCabalBuilderArgs :: Args
sysCabalBuilderArgs = mconcat
   [ builder SysCabalGet ? do
--      p <- expr $ downloadedPath
      mconcat [ arg "get"
              , arg =<< getInput
              , arg "--destdir"
              , arg "/tmp" ] ]

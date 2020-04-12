module Simple.TrustworthyPlugin (plugin) where

import GHC.Plugins
import GHC.Tc.Utils.Monad

plugin :: Plugin
plugin = defaultPlugin
  { renamedResultAction = keepRenamedSource
  , typeCheckResultAction = printHaskellSafeMode
  }
  where
    printHaskellSafeMode _ ms tcg = liftIO $ do
      let dflags = ms_hspp_opts ms
      safe <- finalSafeMode dflags tcg
      print $ gopt Opt_PluginTrustworthy dflags
      putStrLn $ showPpr dflags safe
      return tcg

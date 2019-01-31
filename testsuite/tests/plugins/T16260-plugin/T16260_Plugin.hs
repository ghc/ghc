module T16260_Plugin (plugin) where

import GhcPlugins
import TcRnMonad

plugin :: Plugin
plugin = defaultPlugin
  { pluginTrustworthy = \opts -> return $ null opts
  , renamedResultAction = keepRenamedSource
  , typeCheckResultAction = printHaskellSafeMode
  }
  where
    printHaskellSafeMode opts ms tcg = liftIO $ do
      let dflags = ms_hspp_opts ms
      safe <- finalSafeMode dflags tcg
      print opts
      putStrLn $ showPpr dflags safe
      return tcg

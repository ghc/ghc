module Echo (plugin) where

import GHC.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Utils.Monad
import qualified GHC.Tc.Utils.Monad as Utils
import GHC.Types.Unique.FM ( emptyUFM )

plugin :: Plugin
plugin = mkPureOptTcPlugin optCallCount

mkPureOptTcPlugin :: ([CommandLineOption] -> Maybe Utils.TcPlugin) -> Plugin
mkPureOptTcPlugin p =
    defaultPlugin
        { tcPlugin = p
        , pluginRecompile = impurePlugin
        }

newtype State = State{callref :: IORef Int}

optCallCount :: [CommandLineOption] -> Maybe Utils.TcPlugin
optCallCount opts = Just $
    Utils.TcPlugin
        { tcPluginInit = return . State =<< (unsafeTcPluginTcM $ newMutVar 1)

        , tcPluginSolve = \State{callref = c} _ _ _ -> do
            n <- unsafeTcPluginTcM $ readMutVar c
            let msg = if null opts then "" else mconcat opts
            tcPluginIO . putStrLn $ "Echo TcPlugin " ++ msg ++ "#" ++ show n
            unsafeTcPluginTcM $ writeMutVar c (n + 1)
            return $ TcPluginOk [] []

        , tcPluginRewrite = \ _ -> emptyUFM
        , tcPluginStop = const $ return ()
        }

module TcPlugin_InitStop_Plugin (plugin) where

-- base
import System.IO
  ( hFlush,stdout )

-- ghc
import GHC.Plugins
  ( Plugin(..), defaultPlugin, purePlugin )
import GHC.Tc.Plugin
  ( TcPluginM, getEnvs, tcPluginIO )
import GHC.Tc.Types
  ( TcGblEnv(tcg_mod), TcPlugin(..), TcPluginSolveResult(..) )
import GHC.Types.Unique.FM
  ( emptyUFM )
import GHC.Unit.Module
  ( moduleName, moduleNameString )

plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin = \_ -> Just basicTcPlugin
    , pluginRecompile = purePlugin
    }

basicTcPlugin :: TcPlugin
basicTcPlugin =
  TcPlugin
    { tcPluginInit = do
        mod_name <- currentModuleName
        tcPluginIO $ do
          putStrLn ("Plugin starting for module " ++ mod_name)
          hFlush stdout
        pure mod_name
    , tcPluginSolve = \_ _ _ _ -> pure (TcPluginSolveResult [] [] [])
    , tcPluginRewrite = \_ -> emptyUFM
    , tcPluginPostTc = \mod_name ->
        tcPluginIO $ do
          putStrLn ("Plugin post-tc for module " ++ mod_name)
          hFlush stdout
    , tcPluginShutdown = \mod_name -> do
          putStrLn ("Plugin shutdown for module " ++ mod_name)
          hFlush stdout
    }

currentModuleName :: TcPluginM String
currentModuleName = do
  (tcg_env, _) <- getEnvs
  pure (moduleNameString (moduleName (tcg_mod tcg_env)))

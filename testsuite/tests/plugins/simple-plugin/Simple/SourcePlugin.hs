module Simple.SourcePlugin where

import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Maybe (isJust)
import GHC.Driver.Plugins
import GHC.Driver.Types
import GHC.Tc.Types
import GHC.Hs.Extension
import GHC.Types.Avail
import GHC.Hs.Expr
import GHC.Utils.Outputable
import GHC.Hs.ImpExp
import GHC.Hs.Decls
import GHC.Hs.Doc

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = parsedPlugin
                       , typeCheckResultAction = typecheckPlugin
                       , spliceRunAction = metaPlugin'
                       , interfaceLoadAction = interfaceLoadPlugin'
                       , renamedResultAction = renamedAction
                       }

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule
                  -> Hsc HsParsedModule
parsedPlugin opts _ pm
  = do liftIO $ putStrLn $ "parsePlugin(" ++ intercalate "," opts ++ ")"
       return pm

renamedAction :: [CommandLineOption]
                    -> TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction _ env grp
  = do liftIO $ putStrLn "typeCheckPlugin (rn)"
       return (env, grp)

typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin _ _ tc
  = do liftIO $ putStrLn "typeCheckPlugin (tc)"
       return tc

metaPlugin' :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
metaPlugin' _ meta
  = do liftIO $ putStrLn $ "metaPlugin: " ++ (showSDocUnsafe $ ppr meta)
       return meta

interfaceLoadPlugin' :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin' _ iface
  = do liftIO $ putStrLn $ "interfacePlugin: "
                              ++ (showSDocUnsafe $ ppr $ mi_module iface)
       return iface

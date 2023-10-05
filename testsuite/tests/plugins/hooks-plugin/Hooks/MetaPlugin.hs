{-# OPTIONS_GHC -Wall #-}
module Hooks.MetaPlugin (plugin) where

import GHC.Types.SourceText
import GHC.Plugins
import GHC.Hs.Expr
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Hs.Lit
import GHC.Driver.Hooks
import GHC.Tc.Utils.Monad
import GHC.Parser.Annotation
import System.IO

plugin :: Plugin
plugin = defaultPlugin { driverPlugin = hooksP }

hooksP :: [CommandLineOption] -> HscEnv -> IO HscEnv
hooksP opts hsc_env = do
    let hooks  = hsc_hooks hsc_env
        hooks' = hooks { runMetaHook = Just (fakeRunMeta opts) }
        hsc_env' = hsc_env { hsc_hooks = hooks' }
    return hsc_env'

-- This meta hook doesn't actually care running code in splices,
-- it just replaces any expression splice with the "0"
-- integer literal, and errors out on all other types of
-- meta requests.
fakeRunMeta :: [CommandLineOption] -> MetaHook TcM
fakeRunMeta opts (MetaE r) _ = do
  liftIO . putStrLn $ "Options = " ++ show opts

  -- TODO: Remove #20791
  liftIO $ hFlush stdout

  pure $ r zero

  where zero :: LHsExpr GhcPs
        zero = noLocA $ HsLit noAnn $
          HsInt NoExtField (mkIntegralLit (0 :: Int))

fakeRunMeta _ _ _ = error "fakeRunMeta: unimplemented"

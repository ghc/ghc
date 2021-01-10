{-# OPTIONS_GHC -Wall #-}
module Hooks.Plugin (plugin) where

import GHC.Types.SourceText
import GHC.Plugins
import GHC.Hs.Expr
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension.GhcPass
import GHC.Hs.Lit
import GHC.Driver.Hooks
import GHC.Tc.Utils.Monad

plugin :: Plugin
plugin = defaultPlugin { driverPlugin = hooksP }

hooksP :: [CommandLineOption] -> HscEnv -> IO HscEnv
hooksP opts hsc_env = do
    let dflags  = hsc_dflags hsc_env
        dflags' = dflags
            { hooks = (hooks dflags)
                { runMetaHook = Just (fakeRunMeta opts) }
            }
        hsc_env' = hsc_env { hsc_dflags = dflags' }
    return hsc_env'

-- This meta hook doesn't actually care running code in splices,
-- it just replaces any expression splice with the "0"
-- integer literal, and errors out on all other types of
-- meta requests.
fakeRunMeta :: [CommandLineOption] -> MetaHook TcM
fakeRunMeta opts (MetaE r) _ = do
  liftIO . putStrLn $ "Options = " ++ show opts
  pure $ r zero

  where zero :: LHsExpr GhcPs
        zero = L noSrcSpan $ HsLit NoExtField $
          HsInt NoExtField (mkIntegralLit (0 :: Int))

fakeRunMeta _ _ _ = error "fakeRunMeta: unimplemented"

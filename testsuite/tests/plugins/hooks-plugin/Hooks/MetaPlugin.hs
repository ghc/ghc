{-# OPTIONS_GHC -Wall #-}
module Hooks.MetaPlugin (plugin) where

import Control.Monad (when)
import GHC.Types.SourceText
import GHC.Plugins
import GHC.Hs.Expr
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Hs.Lit
import GHC.Driver.Hooks
import GHC.Tc.Gen.Splice (defaultRunMeta)
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

-- Replace expression splices with zero. Delegate type splices to GHC.
fakeRunMeta :: [CommandLineOption] -> MetaHook TcM
fakeRunMeta opts (MetaE r) _ = do
  liftIO . putStrLn $ "Options = " ++ show opts

  -- TODO: Remove #20791
  liftIO $ hFlush stdout

  pure $ r zero

  where zero :: LHsExpr GhcPs
        zero = noLocA $ HsLit noExtField $
          HsInt NoExtField (mkIntegralLit (0 :: Int))

fakeRunMeta opts req@MetaT{} expr = do
  result <- defaultRunMeta req expr
  when ("trace-type" `elem` opts) $
    liftIO $ hPutStrLn stderr "Forwarded type splice"
  pure result
fakeRunMeta _ _ _ = error "fakeRunMeta: unimplemented"

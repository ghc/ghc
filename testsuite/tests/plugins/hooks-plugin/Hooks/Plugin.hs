{-# OPTIONS_GHC -Wall #-}
module Hooks.Plugin (plugin) where

import BasicTypes
import Hooks
import HsExpr
import HsExtension
import HsLit
import HscTypes
import Plugins
import SrcLoc
import TcRnMonad

plugin :: Plugin
plugin = defaultPlugin { hooksPlugin = hooksP }

hooksP :: [CommandLineOption] -> Maybe (Hooks -> Hooks)
hooksP _opts = Just $ \h -> h { runMetaHook = Just fakeRunMeta }

-- This meta hook doesn't actually care running code in splices,
-- it just replaces any expression splice with the "0"
-- integer literal, and errors out on all other types of
-- meta requests.
fakeRunMeta :: MetaRequest -> LHsExpr GhcTc -> TcM MetaResult
fakeRunMeta (MetaE r) = const (pure $ r zero)

  where zero :: LHsExpr GhcPs
        zero = L noSrcSpan $ HsLit NoExtField $ HsInt NoExtField (mkIntegralLit (0 :: Int))

fakeRunMeta _ = error "fakeRunMeta: unimplemented"

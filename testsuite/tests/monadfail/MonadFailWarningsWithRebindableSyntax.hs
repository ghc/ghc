-- Test purpose:
-- RebindableSyntax does not play that well with MonadFail, so here we ensure
-- that when both settings are enabled we get the proper warning.

{-# OPTIONS_GHC -fwarn-missing-monadfail-instance #-}
{-# LANGUAGE RebindableSyntax #-}

module MonadFailWarningsWithRebindableSyntax where

import Prelude

test1 f g = do
    Just x <- f
    g

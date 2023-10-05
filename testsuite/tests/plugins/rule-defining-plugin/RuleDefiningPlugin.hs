module RuleDefiningPlugin where

import GHC.Plugins

{-# RULES "unsound" forall x. show x = "SHOWED" #-}

plugin :: Plugin
plugin = defaultPlugin

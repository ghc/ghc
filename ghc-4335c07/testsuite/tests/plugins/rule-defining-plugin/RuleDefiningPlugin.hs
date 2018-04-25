module RuleDefiningPlugin where

import GhcPlugins

{-# RULES "unsound" forall x. show x = "SHOWED" #-}

plugin :: Plugin
plugin = defaultPlugin

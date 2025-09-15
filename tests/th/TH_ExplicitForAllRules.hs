{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}

module Main where

import TH_ExplicitForAllRules_a

$(decls)

main = hsToTh

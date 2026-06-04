{-# OPTIONS_GHC -fno-prof-auto #-}

module Main (main) where

import Control.Exception.Backtrace

{-# SCC bottom #-}
bottom :: IO ()
bottom = do
    bt <- collectBacktraces
    putStrLn $ displayBacktraces bt

{-# SCC middle #-}
middle :: IO ()
middle = bottom

{-# SCC top #-}
top :: IO ()
top = middle

{-# SCC main #-}
main :: IO ()
main = do
    setBacktraceMechanismState HasCallStackBacktrace False
    setBacktraceMechanismState ExecutionBacktrace    False
    setBacktraceMechanismState IPEBacktrace          False
    setBacktraceMechanismState CostCentreBacktrace   True
    top

{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-prof-auto #-}

module Main (main) where

import Control.Exception.Backtrace


type family MyIO a where
  MyIO () = IO ()

{-# SCC bottom #-}
bottom :: MyIO ()
bottom = do
    bt <- collectBacktraces
    putStrLn $ displayBacktraces bt

{-# SCC middle #-}
middle :: MyIO ()
middle = bottom

{-# SCC top #-}
top :: MyIO ()
top = middle

{-# SCC main #-}
main :: MyIO ()
main = do
    setBacktraceMechanismState HasCallStackBacktrace False
    setBacktraceMechanismState ExecutionBacktrace    False
    setBacktraceMechanismState IPEBacktrace          False
    setBacktraceMechanismState CostCentreBacktrace   True
    top

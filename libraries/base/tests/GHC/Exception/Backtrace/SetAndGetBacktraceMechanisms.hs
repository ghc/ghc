module Main where

import GHC.Exception.Backtrace
import System.Exit (die)
import Prelude

main = do
  expectDefaultBacktraceMechanisms []

  setAndExpectDefaultBacktraceMechanisms []

  setAndExpectDefaultBacktraceMechanisms [CostCenterBacktraceMech]

  setAndExpectDefaultBacktraceMechanisms [ExecutionStackBacktraceMech]

  setAndExpectDefaultBacktraceMechanisms
    [ CostCenterBacktraceMech,
      ExecutionStackBacktraceMech
    ]

  setAndExpectDefaultBacktraceMechanisms
    [ CostCenterBacktraceMech,
      ExecutionStackBacktraceMech,
      HasCallStackBacktraceMech,
      IPEBacktraceMech
    ]

setAndExpectDefaultBacktraceMechanisms :: [BacktraceMechanism] -> IO ()
setAndExpectDefaultBacktraceMechanisms bts = do
  setDefaultBacktraceMechanisms bts
  expectDefaultBacktraceMechanisms bts

expectDefaultBacktraceMechanisms :: [BacktraceMechanism] -> IO ()
expectDefaultBacktraceMechanisms expected = do
  ms <- getDefaultBacktraceMechanisms
  if ms /= expected
    then die $ "Expected " ++ show expected ++ " as default backtrace mechanisms, but got: " ++ show ms
    else return ()

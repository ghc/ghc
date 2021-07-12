module Main where

import Prelude
import System.Exit (die)
import GHC.Exception.Backtrace

main = do
    ms <- getDefaultBacktraceMechanisms
    if (not . null) ms then
        die $ "Expected empty default backtrace mechanisms, but got: " ++ show ms
    else
        return ()

    let expected = [CostCenterBacktraceMech]
    setDefaultBacktraceMechanisms expected
    ms' <- getDefaultBacktraceMechanisms
    if ms' /= expected then
        die $ "Expected " ++ show expected ++" as default backtrace mechanisms, but got: " ++ show ms'
    else
        return ()

    let expected' = [ExecutionStackBacktraceMech (Just 42)]
    setDefaultBacktraceMechanisms expected'
    ms'' <- getDefaultBacktraceMechanisms
    if ms'' /= expected' then
        die $ "Expected " ++ show expected' ++" as default backtrace mechanisms, but got: " ++ show ms''
    else
        return ()

    let expected'' = [CostCenterBacktraceMech, ExecutionStackBacktraceMech (Just 42)]
    setDefaultBacktraceMechanisms expected''
    ms''' <- getDefaultBacktraceMechanisms
    if ms''' /= expected'' then
        die $ "Expected " ++ show expected'' ++ " as default backtrace mechanisms, but got: " ++ show ms'''
    else
        return ()

module Supercompile.StaticFlags where

import System.Environment
import System.IO.Unsafe


{-# NOINLINE aRGS #-}
aRGS :: [String]
aRGS = unsafePerformIO getArgs

cALL_BY_NAME :: Bool
cALL_BY_NAME = "--call-by-name" `elem` aRGS

dEEDS :: Bool
dEEDS = "--deeds" `elem` aRGS

dUPLICATE_VALUES_EVALUATOR, dUPLICATE_VALUES_SPLITTER :: Bool
dUPLICATE_VALUES_EVALUATOR = "--duplicate-values-evaluator" `elem` aRGS
dUPLICATE_VALUES_SPLITTER = "--duplicate-values-splitter" `elem` aRGS

oCCURRENCE_GENERALISATION :: Bool
oCCURRENCE_GENERALISATION = not $ "--no-occurrence-generalisation" `elem` aRGS

eVALUATE_PRIMOPS :: Bool
eVALUATE_PRIMOPS = not $ "--no-primops" `elem` aRGS

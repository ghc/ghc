module Supercompile.StaticFlags where

import Data.Char (toLower)
import Data.Maybe
import Data.List (stripPrefix)

import System.Environment
import System.IO.Unsafe


{-# NOINLINE aRGS #-}
aRGS :: [String]
aRGS = unsafePerformIO getArgs

parseEnum :: String -> a -> [(String, a)] -> a
parseEnum prefix def opts = fromMaybe def $ listToMaybe [parse opt | arg <- aRGS, Just ('=':opt) <- [stripPrefix prefix arg]]
  where parse = fromJust . flip lookup opts . map toLower


dEEDS :: Bool
dEEDS = "--deeds" `elem` aRGS

data DeedsPolicy = FCFS | Proportional
                 deriving (Read)

dEEDS_POLICY :: DeedsPolicy
dEEDS_POLICY = parseEnum "--deeds-policy" Proportional [("fcfs", FCFS), ("proportional", Proportional)]


cALL_BY_NAME :: Bool
cALL_BY_NAME = "--call-by-name" `elem` aRGS


dUPLICATE_VALUES_EVALUATOR, dUPLICATE_VALUES_SPLITTER :: Bool
dUPLICATE_VALUES_EVALUATOR = "--duplicate-values-evaluator" `elem` aRGS
dUPLICATE_VALUES_SPLITTER = "--duplicate-values-splitter" `elem` aRGS

data GeneralisationType = NoGeneralisation | AllEligible | DependencyOrder Bool | StackFirst

gENERALISATION :: GeneralisationType
gENERALISATION = parseEnum "--generalisation" StackFirst [("none", NoGeneralisation), ("all-eligible", AllEligible), ("first-reachable", DependencyOrder True), ("last-reachable", DependencyOrder False), ("stack-first", StackFirst)]

oCCURRENCE_GENERALISATION :: Bool
oCCURRENCE_GENERALISATION = not $ "--no-occurrence-generalisation" `elem` aRGS

eVALUATE_PRIMOPS :: Bool
eVALUATE_PRIMOPS = not $ "--no-primops" `elem` aRGS

sPECULATION :: Bool
sPECULATION = not $ "--no-speculation" `elem` aRGS

lOCAL_TIEBACKS :: Bool
lOCAL_TIEBACKS = "--local-tiebacks" `elem` aRGS

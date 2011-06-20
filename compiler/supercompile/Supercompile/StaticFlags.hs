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

bLOAT_FACTOR :: Int
bLOAT_FACTOR = fromMaybe 10 $ listToMaybe [read val | arg <- aRGS, Just val <- [stripPrefix "--bloat=" arg]]
 -- NB: need a bloat factor of at least 5 to get append/append fusion to work. The critical point is:
 --
 --  let (++) = ...
 --  in case (case xs of []     -> ys
 --                      (x:xs) -> x : (xs ++ ys)) of
 --    []     -> zs
 --    (x:xs) -> x : (xs ++ zs)
 --
 -- We need to duplicate the case continuation into each branch, so at one time we will have:
 --  1) Two copies of (++) in the [] branch of the inner case
 --    a) One in the heap
 --    b) One from the stack (from [_] ++ zs)
 --  2) Similarly two copies in the (:) branch of the inner case
 --  3) One copy manifested in the residual branch of xs
 --
 -- Total = 5 copies (due to tiebacks, the residual program will do better than this)
 --
 -- 
 -- Unfortunately, my implementation doesn't tie back as eagerly as you might like, so we actually peel the loop once and
 -- hence need a bloat factor of 8 here (5 + 3 other case statements derived from (++))
 -- TODO: figure out how to reduce this number.


cALL_BY_NAME :: Bool
cALL_BY_NAME = "--call-by-name" `elem` aRGS


dUPLICATE_VALUES_EVALUATOR, dUPLICATE_VALUES_SPLITTER :: Bool
dUPLICATE_VALUES_EVALUATOR = "--duplicate-values-evaluator" `elem` aRGS
dUPLICATE_VALUES_SPLITTER = "--duplicate-values-splitter" `elem` aRGS


data TagBagType = TBT { tagBagPairwiseGrowth :: Bool }
                deriving (Show)

tAG_COLLECTION :: TagBagType
tAG_COLLECTION = parseEnum "--tag-collection" (TBT False) [("bags", TBT False), ("bags-strong", TBT True)]


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

rEFINE_FULFILMENT_FVS :: Bool
rEFINE_FULFILMENT_FVS = not $ "--no-refine-fulfilment-fvs" `elem` aRGS


rEDUCE_ROLLBACK :: Bool
rEDUCE_ROLLBACK = not $ "--no-reduce-rollback" `elem` aRGS

sC_ROLLBACK :: Bool
sC_ROLLBACK = not $ "--no-sc-rollback" `elem` aRGS

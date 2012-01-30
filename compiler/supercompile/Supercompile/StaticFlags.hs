module Supercompile.StaticFlags where

import Data.Char (toLower)
import Data.Maybe

import FastString
import StaticFlags


parseEnum :: String -> a -> [(String, a)] -> a
parseEnum prefix def opts = maybe def parse $ lookup_str prefix
  where parse = fromJust . flip lookup opts . map toLower


-- The StaticFlagsParser admits any option beginning with -fsupercompiler


iNSTANCE_MATCHING :: Bool
iNSTANCE_MATCHING = not $ lookUp $ fsLit "-fsupercompiler-no-instance-matching"

eAGER_SPLIT_VALUES :: Bool
eAGER_SPLIT_VALUES = iNSTANCE_MATCHING -- For correctness given that we do instance matching
--eAGER_SPLIT_VALUES = False

rEFINE_ALTS :: Bool
rEFINE_ALTS = not $ lookUp $ fsLit "-fsupercompiler-no-refine-alts"
--rEFINE_ALTS = False

dEEDS :: Bool
dEEDS = lookUp $ fsLit "-fsupercompiler-deeds"
--dEEDS = True

bOUND_STEPS :: Bool
bOUND_STEPS = lookUp $ fsLit "-fsupercompiler-bound-steps"
--bOUND_STEPS = True

-- For debugging very long-running supercompilation
dEPTH_LIIMT :: Maybe Int
dEPTH_LIIMT = Just (lookup_def_int "-fsupercompiler-depth-limit" maxBound)
--dEPTH_LIIMT = Just 10

pOSITIVE_INFORMATION :: Bool
pOSITIVE_INFORMATION = lookUp $ fsLit "-fsupercompiler-positive-information"
--pOSITIVE_INFORMATION = True

data DeedsPolicy = FCFS | Proportional
                 deriving (Read)

dEEDS_POLICY :: DeedsPolicy
dEEDS_POLICY = parseEnum "-fsupercompiler-deeds-policy" Proportional [("fcfs", FCFS), ("proportional", Proportional)]

bLOAT_FACTOR :: Int
--bLOAT_FACTOR = fromMaybe 10 $ listToMaybe [read val | arg <- aRGS, Just val <- [stripPrefix "--bloat=" arg]]
bLOAT_FACTOR = lookup_def_int "-fsupercompiler-bloat-factor" 10
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
cALL_BY_NAME = lookUp $ fsLit "-fsupercompiler-call-by-name"


dUPLICATE_VALUES_EVALUATOR, dUPLICATE_VALUES_SPLITTER :: Bool
dUPLICATE_VALUES_EVALUATOR = lookUp $ fsLit "-fsupercompiler-duplicate-values-evaluator"
dUPLICATE_VALUES_SPLITTER = lookUp $ fsLit "-fsupercompiler-duplicate-values-splitter"


data TagBagType = TBT { tagBagPairwiseGrowth :: Bool }
                deriving (Show)

tAG_COLLECTION :: TagBagType
tAG_COLLECTION = parseEnum "-fsupercompiler-tag-collection" (TBT False) [("bags", TBT False), ("bags-strong", TBT True)]


data GeneralisationType = NoGeneralisation | AllEligible | DependencyOrder Bool | StackFirst

gENERALISATION :: GeneralisationType
gENERALISATION = parseEnum "-fsupercompiler-generalisation" StackFirst [("none", NoGeneralisation), ("all-eligible", AllEligible), ("first-reachable", DependencyOrder True), ("last-reachable", DependencyOrder False), ("stack-first", StackFirst)]

oCCURRENCE_GENERALISATION :: Bool
oCCURRENCE_GENERALISATION = not $ lookUp $ fsLit "-fsupercompiler-no-occurrence-generalisation"


eVALUATE_PRIMOPS :: Bool
eVALUATE_PRIMOPS = not $ lookUp $ fsLit "-fsupercompiler-no-primops"

sPECULATION :: Bool
sPECULATION = not $ lookUp $ fsLit "-fsupercompiler-no-speculation"

lOCAL_TIEBACKS :: Bool
lOCAL_TIEBACKS = lookUp $ fsLit "-fsupercompiler-local-tiebacks"

rEFINE_FULFILMENT_FVS :: Bool
rEFINE_FULFILMENT_FVS = not $ lookUp $ fsLit "-fsupercompiler-no-refine-fulfilment-fvs"


rEDUCE_ROLLBACK :: Bool
rEDUCE_ROLLBACK = not $ lookUp $ fsLit "-fsupercompiler-no-reduce-rollback"

sC_ROLLBACK :: Bool
sC_ROLLBACK = not $ lookUp $ fsLit "-fsupercompiler-no-sc-rollback"

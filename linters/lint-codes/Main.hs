{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- base
import Control.Monad
  ( when )
import Data.List
  ( sortOn )
import Text.Printf
  ( printf )
import System.Environment
  ( getArgs )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( (\\), intersection, mapMaybe, toList, withoutKeys )

-- ghc
import GHC.Types.Error
  ( DiagnosticCode(..) )

-- lint-codes
import LintCodes.Args
  ( Mode(..), parseArgs )
import LintCodes.Static
  ( FamEqnIndex, Use, used, outdated
  , getFamEqnCodes
  , staticallyUsedCodes
  )
import LintCodes.Coverage
  ( getCoveredCodes )

--------------------------------------------------------------------------------

main :: IO ()
main = do

  args <- getArgs
  let !(!mode, mb_libDir) = parseArgs args

  famEqnCodes <- getFamEqnCodes mb_libDir

  case mode of
    Test     -> testCodes         famEqnCodes
    List     -> listCodes         famEqnCodes
    Outdated -> listOutdatedCodes famEqnCodes

-- | List all statically used diagnostic codes.
listCodes :: Map DiagnosticCode ( FamEqnIndex, String, Use ) -> IO ()
listCodes famEqnCodes = do
  let usedCodes = Map.mapMaybe used famEqnCodes
                 `Map.intersection` staticallyUsedCodes
  putStrLn $ showDiagnosticCodesWith printCode usedCodes

-- | List all outdated diagnostic codes.
listOutdatedCodes :: Map DiagnosticCode ( FamEqnIndex, String, Use ) -> IO ()
listOutdatedCodes famEqnCodes = do
  let outdatedCodes = Map.mapMaybe outdated famEqnCodes
  putStrLn $ showDiagnosticCodesWith printCode outdatedCodes

-- | Test consistency and coverage of diagnostic codes.
--
-- Assumes we are in a GHC Git tree, as we look at all testsuite .stdout and
-- .stderr files.
testCodes :: Map DiagnosticCode ( FamEqnIndex, String, Use ) -> IO ()
testCodes famEqnCodes = do

  ------------------------------
  -- Static consistency checks.

  let
    familyEqnUsedCodes = Map.mapMaybe used famEqnCodes
    familyEqnOutdatedCodes = Map.mapMaybe outdated famEqnCodes

    -- Consistency of diagnostic codes:
    --   all diagnostic codes returned by the 'GhcDiagnosticCode' type family
    --   should be statically used, unless they are marked as outdated.
    staticallyUnusedCodes = familyEqnUsedCodes Map.\\ staticallyUsedCodes

    -- Consistency of outdated diagnostic codes:
    --   if a diagnostic code is marked as outdated, it should not be statically used.
    outdatedStaticallyUsedCodes =
      familyEqnOutdatedCodes `Map.intersection` staticallyUsedCodes

  -- Test 1: all non-outdated 'GhcDiagnosticCode' equations are statically used.
  let plural1 = length staticallyUnusedCodes > 1
      test1OK :: Bool
      test1Message :: String
      (test1OK, test1Message)
        | null staticallyUnusedCodes
        = (True,) $
          "- All non-outdated 'GhcDiagnosticCode' equations are statically used."
        | otherwise
        = (False,) $
          unlines [ "- The following 'GhcDiagnosticCode' equation" ++ (if plural1 then "s appear" else " appears") ++ " to be unused."
                  , "  If " ++ (if plural1 then "any of these codes are indeed no longer used, but were"
                                           else "this code is indeed no longer used, but was")
                  , "  emitted by a previous version of GHC, you should mark " ++ (if plural1 then "them" else "it") ++ " as outdated"
                  , "  by tagging the RHS of the appropriate type family equation of"
                  , "  the 'GhcDiagnosticCode' type family in 'GHC.Types.Error.Codes'"
                  , "  with the 'Outdated' type synonym."
                  , ""
                  , showDiagnosticCodesWith printUnused staticallyUnusedCodes
                  ]
  putStrLn ""
  putStrLn test1Message
  putStrLn ""

  -- Test 2: all outdated 'GhcDiagnosticCode' equations are statically unused.
  let plural2 = length outdatedStaticallyUsedCodes > 1
      test2OK :: Bool
      test2Message :: String
      (test2OK, test2Message)
        | null outdatedStaticallyUsedCodes
        = (True,) $
          "- All outdated 'GhcDiagnosticCode' equations are statically unused."
        | otherwise
        = (False,) $
          unlines [ "- The following 'GhcDiagnosticCode' equation" ++ (if plural2 then "s are" else " is") ++ " still in use,"
                  , "  even though " ++ (if plural2 then "they are" else "it is") ++ " marked as being outdated."
                  , "  Perhaps you should remove the 'Outdated' tag on " ++ (if plural2 then "them" else "it") ++ "."
                  , ""
                  , showDiagnosticCodesWith printOutdatedUsed outdatedStaticallyUsedCodes
                  ]
  putStrLn test2Message
  putStrLn ""

  -------------------------
  -- Code coverage checks.

  -- Test 3: all statically used diagnostic codes are covered by the testsuite,
  -- (exceptions are allowed in the test output).
  coveredCodes <- getCoveredCodes
  when ( null coveredCodes ) $
    error $ unlines [ "internal error in 'lint-codes' test:"
                    , "  failed to parse any diagnostic codes from the testsuite"
                    ]

  let uncoveredCodes :: Map DiagnosticCode (FamEqnIndex, String)
      uncoveredCodes = (familyEqnUsedCodes `Map.intersection` staticallyUsedCodes)
                     `Map.withoutKeys` coveredCodes
      plural3 = length uncoveredCodes > 1
      test3OK :: Bool
      test3Message :: String
      (test3OK, test3Message)
        | null uncoveredCodes
        = (True,) $
          "- All diagnostic codes are covered by the testsuite."
        | otherwise
        = (False,) $
          unlines [ "- The following diagnostic code" ++ (if plural3 then "s seem" else " seems") ++ " to not be covered by any tests,"
                  , "  as determined by analysing all '.stderr' and '.stdout' files in the testsuite."
                  , "  If there is a change in the expected output of this test, you can:"
                  , "    - add test cases to exercise any newly uncovered diagnostic codes,"
                  , "    - accept the expected output of the 'codes' test by passing the '-a' flag to Hadrian."
                  , ""
                  , showDiagnosticCodesWith printUntested uncoveredCodes
                  ]

  putStrLn test3Message
  when (test1OK && test2OK && test3OK) do
    putStrLn ""
    putStrLn "All good!"

-- | Show a collection of diagnostic codes, ordered by the index in which
-- the diagnostic code appears in the 'GhcDiagnosticCode' type family.
showDiagnosticCodesWith :: ( (DiagnosticCode, String) -> String )
                           -- ^ how to print each diagnostic code
                        -> Map DiagnosticCode (FamEqnIndex, String) -> String
showDiagnosticCodesWith f codes = unlines $ map showCodeCon $ sortOn famEqnIndex $ Map.toList codes
  where
    showCodeCon :: (DiagnosticCode, (FamEqnIndex, String)) -> String
    showCodeCon (code, (_, con)) = f (code, con)
    famEqnIndex :: (DiagnosticCode, (FamEqnIndex, String)) -> FamEqnIndex
    famEqnIndex (_, (i,_)) = i

printUnused, printOutdatedUsed, printUntested, printCode :: (DiagnosticCode, String) -> String
printUnused (code, con) =
  "Unused equation: GhcDiagnosticCode " ++ show con ++ " = " ++ showDiagnosticCodeNumber code
printOutdatedUsed (code, con) =
  "Outdated equation is used: GhcDiagnosticCode " ++ show con ++ " = Outdated " ++ showDiagnosticCodeNumber code
printUntested (code, con) =
  "[" ++ show code ++ "] is untested (constructor = " ++ con ++ ")"
printCode (code, con) =
  "[" ++ show code ++ "] " ++ show con

showDiagnosticCodeNumber :: DiagnosticCode -> String
showDiagnosticCodeNumber (DiagnosticCode { diagnosticCodeNumber = c })
  = printf "%05d" c

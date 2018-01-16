module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Data.Algorithm.Diff                         (Diff (..), getGroupedDiff)
import Data.Maybe                                  (isJust)
import Distribution.License                        (License (..))
import Distribution.PackageDescription             (GenericPackageDescription)
import Distribution.PackageDescription.Parsec      (parseGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec.Common                  (PWarnType (..), PWarning (..))
import Distribution.Parsec.ParseResult             (runParseResult)
import Distribution.Utils.Generic                  (fromUTF8BS, toUTF8BS)
import System.FilePath                             (replaceExtension, (</>))

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import           Distribution.Compat.Lens
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens        as L

tests :: TestTree
tests = testGroup "parsec tests"
    [ warningTests
    , regressionTests
    ]

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

-- Verify that we trigger warnings
warningTests :: TestTree
warningTests = testGroup "warnings triggered"
    [ warningTest PWTLexBOM            "bom.cabal"
    , warningTest PWTLexNBSP           "nbsp.cabal"
    , warningTest PWTUTF               "utf8.cabal"
    , warningTest PWTBoolCase          "bool.cabal"
    , warningTest PWTVersionTag        "versiontag.cabal"
    , warningTest PWTNewSyntax         "newsyntax.cabal"
    , warningTest PWTOldSyntax         "oldsyntax.cabal"
    , warningTest PWTDeprecatedField   "deprecatedfield.cabal"
    , warningTest PWTInvalidSubsection "subsection.cabal"
    , warningTest PWTUnknownField      "unknownfield.cabal"
    , warningTest PWTUnknownSection    "unknownsection.cabal"
    , warningTest PWTTrailingFields    "trailingfield.cabal"
    -- TODO: not implemented yet
    -- , warningTest PWTExtraTestModule   "extratestmodule.cabal"
    ]

warningTest :: PWarnType -> FilePath -> TestTree
warningTest wt fp = testCase (show wt) $ do
    contents <- BS.readFile $ "tests" </> "ParserTests" </> "warnings" </> fp

    let res =  parseGenericPackageDescription contents
    let (warns, errs, x) = runParseResult res

    assertBool ("should parse successfully: " ++ show errs) $ isJust x
    assertBool ("should parse without errors:  " ++  show errs) $ null errs

    case warns of
        [PWarning wt' _ _] -> assertEqual "warning type" wt wt'
        []                 -> assertFailure "got no warnings"
        _                  -> assertFailure $ "got multiple warnings: " ++ show warns

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

regressionTests :: TestTree
regressionTests = testGroup "regressions"
    [ regressionTest "encoding-0.8.cabal"
    , regressionTest "Octree-0.5.cabal"
    , regressionTest "nothing-unicode.cabal"
    , regressionTest "issue-774.cabal"
    , regressionTest "generics-sop.cabal"
    , regressionTest "elif.cabal"
    , regressionTest "elif2.cabal"
    , regressionTest "shake.cabal"
    ]

regressionTest :: FilePath -> TestTree
regressionTest fp = testGroup fp
    [ formatGoldenTest fp
    , formatRoundTripTest fp
    ]

formatGoldenTest :: FilePath -> TestTree
formatGoldenTest fp = cabalGoldenTest "format" correct $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (_, errs, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Just gpd | null errs ->
            showGenericPackageDescription gpd
        _ ->
            unlines $ "ERROR" : map show errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "format"

formatRoundTripTest :: FilePath -> TestTree
formatRoundTripTest fp = testCase "roundtrip" $ do
    contents <- BS.readFile input
    x <- parse contents
    let contents' = showGenericPackageDescription x
    y <- parse (toUTF8BS contents')
    -- 'License' type doesn't support parse . pretty roundrip (yet).
    -- Will be fixed when we refactor to SPDX
    let y' = if x ^. L.packageDescription . L.license == UnspecifiedLicense
                && y ^. L.packageDescription . L.license == UnknownLicense "UnspecifiedLicense"
             then y & L.packageDescription . L.license .~ UnspecifiedLicense
             else y
    assertEqual "re-parsed doesn't match" x y'
  where
    parse :: BS.ByteString -> IO GenericPackageDescription
    parse c = do
        let (_, errs, x') = runParseResult $ parseGenericPackageDescription c
        case x' of
            Just gpd | null errs -> return gpd
            _                    -> do
                assertFailure $ unlines (map show errs)
                fail "failure"
    input = "tests" </> "ParserTests" </> "regressions" </> fp

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

cabalGoldenTest :: TestName -> FilePath -> IO BS.ByteString -> TestTree
cabalGoldenTest name ref act = goldenTest name (BS.readFile ref) act cmp upd
  where
    upd = BS.writeFile ref
    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ unlines $
        concatMap f (getGroupedDiff (BS8.lines x) (BS8.lines y))
      where
        f (First xs)  = map (cons3 '-' . fromUTF8BS) xs
        f (Second ys) = map (cons3 '+' . fromUTF8BS) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . fromUTF8BS) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs

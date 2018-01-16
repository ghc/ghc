module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

import Data.Algorithm.Diff                    (Diff (..), getGroupedDiff)
import Distribution.PackageDescription.Check  (checkPackage)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.ParseResult        (runParseResult)
import Distribution.Utils.Generic             (fromUTF8BS, toUTF8BS)
import System.FilePath                        (replaceExtension, (</>))

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

tests :: TestTree
tests = checkTests

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

checkTests :: TestTree
checkTests = testGroup "regressions"
    [ checkTest "nothing-unicode.cabal"
    , checkTest "haddock-api-2.18.1-check.cabal"
    ]

checkTest :: FilePath -> TestTree
checkTest fp = cabalGoldenTest fp correct $ do
    contents <- BS.readFile input
    let res =  parseGenericPackageDescription contents
    let (_, errs, x) = runParseResult res

    return $ toUTF8BS $ case x of
        Just gpd | null errs ->
            unlines $ map show (checkPackage gpd Nothing)
        _ ->
            unlines $ "ERROR" : map show errs
  where
    input = "tests" </> "ParserTests" </> "regressions" </> fp
    correct = replaceExtension input "check"

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

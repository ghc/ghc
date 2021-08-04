{-# LANGUAGE OverloadedStrings #-}

module QC.Text.Regressions (
      tests
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Char (isLower)
import Data.Monoid ((<>))
import QC.Rechunked (rechunkT)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, counterexample, forAll)
import qualified Data.Attoparsec.Text as A


--------------------------------------------------------------------------------
-- 105 was about runScanner not always returning the final state. The result
-- did depend on how the data was fed to the parser.

t_issue105 :: Property
t_issue105 = expect issue105 "lowER" (A.Done "ER" "low")

issue105 :: A.Parser Text
issue105 = do
    (_, firstFourLowercaseLetters) <- A.runScanner "" f
    return $ firstFourLowercaseLetters

  where
    f :: Text -> Char -> Maybe Text
    f acc c = if T.length acc < 4 && isLower c
        then Just $ acc <> T.singleton c
        else Nothing


expect :: (Show r, Eq r) => A.Parser r -> Text -> A.Result r -> Property
expect p input wanted =
  forAll (rechunkT input) $ \in' ->
    let result = parse p in'
    in counterexample (show result ++ " /= " ++ show wanted) $
       fromMaybe False (A.compareResults result wanted)

parse :: A.Parser r -> [Text] -> A.Result r
parse p (x:xs) = foldl' A.feed (A.parse p x) xs
parse p []     = A.parse p ""


tests :: [TestTree]
tests = [
      testProperty "issue105" t_issue105
  ]

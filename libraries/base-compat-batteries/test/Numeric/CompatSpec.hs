module Numeric.CompatSpec (main, spec) where

import Test.Hspec
import Numeric.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showFFloatAlt" $ do
    it "shows a RealFloat value, always using decimal notation" $
      showFFloatAlt Nothing  (12 :: Double) "" `shouldBe` "12.0"
    it "allows to specify the number of decimal places" $
      showFFloatAlt (Just 4) (12 :: Double) "" `shouldBe` "12.0000"
  describe "showGFloatAlt" $ do
    it "shows a RealFloat value, using decimal notation if the absolute value lies between 0.1 and 9,999,999" $
      showGFloatAlt Nothing  (12 :: Double) ""         `shouldBe` "12.0"
    it "shows a RealFloat value, using decimal notation and specifying the number of decimal places" $
      showGFloatAlt (Just 4) (12 :: Double) ""         `shouldBe` "12.0000"
    it "shows a RealFloat value, using scientific notation if the absolute value falls outside of the range" $
      showGFloatAlt Nothing  (1234567890 :: Double) "" `shouldBe` "1.23456789e9"
    it "shows a RealFloat value, using scientific notation and specifying the number of decimal places" $
      showGFloatAlt (Just 4) (1234567890 :: Double) "" `shouldBe` "1.2346e9"

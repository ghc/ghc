module Data.STRef.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "modifySTRef'" $
    it "should mutate the contents of an STRef strictly" $
      shouldBe (1000000 :: Int) $ runST $ do
        ref <- newSTRef 0
        replicateM_ 1000000 $ modifySTRef' ref (+1)
        readSTRef ref

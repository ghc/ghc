module Foreign.Marshal.Alloc.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Exception
import           Foreign.Marshal.Alloc.Compat
import           Foreign.Storable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "calloc" $
    it "allocates memory with bytes of value zero" $ do
      bracket calloc free $ \ptr -> do
        peek ptr `shouldReturn` (0 :: Int)

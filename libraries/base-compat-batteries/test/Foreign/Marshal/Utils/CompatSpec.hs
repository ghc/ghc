module Foreign.Marshal.Utils.CompatSpec (main, spec) where

import Test.Hspec

import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils.Compat
import Foreign.Ptr
import Foreign.Storable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fillBytes" $
    it "fills a given number of bytes in memory area with a byte value" $ do
      alloca $ \ptr -> do
        let _ = ptr :: Ptr Int
        fillBytes ptr 0 $ sizeOf ptr
        peek ptr `shouldReturn` 0

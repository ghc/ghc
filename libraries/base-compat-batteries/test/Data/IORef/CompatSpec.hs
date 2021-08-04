module Data.IORef.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Monad
import           Data.IORef.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "modifyIORef'" $
    it "mutates the contents of an IORef strictly" $ do
      ref <- newIORef 0
      replicateM_ 1000000 $ modifyIORef' ref (+1)
      readIORef ref `shouldReturn` (1000000 :: Int)
  describe "atomicModifyIORef'" $
    it "atomically modifies the contents of an IORef strictly" $ do
      ref <- newIORef 0
      replicateM_ 1000000 . atomicModifyIORef' ref $ \n -> (n+1, ())
      readIORef ref `shouldReturn` (1000000 :: Int)

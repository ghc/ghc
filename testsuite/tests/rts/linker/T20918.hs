{-# LANGUAGE ForeignFunctionInterface #-}

module ThrowTest where

import Foreign.C.Types
import Data.Int()

foreign import ccall "test_error_throwing.h equivalent_to_id"
  equivalentToId :: CDouble -> IO CDouble

hello :: IO ()
hello = do
  print "Hi"
  shouldBe88 <- equivalentToId 88
  print $ show shouldBe88
  print "Bye"


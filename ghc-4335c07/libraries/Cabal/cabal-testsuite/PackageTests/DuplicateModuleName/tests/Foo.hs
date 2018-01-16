{-# LANGUAGE PackageImports #-}
module Foo where

import Distribution.TestSuite
import qualified "DuplicateModuleName" Foo as T

tests :: IO [Test]
tests = do
    r <- T.tests
    return $ [Test $ TestInstance
          { run = return (Finished (Fail "B"))
          , name = "test B"
          , tags = []
          , options = []
          , setOption = \_ _-> Left "No Options"
          }] ++ r

this_is_test = True

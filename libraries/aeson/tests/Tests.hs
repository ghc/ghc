{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Prelude.Compat

import Test.Tasty (defaultMain, testGroup)
import qualified DataFamilies.Properties as DF
import qualified Properties
import qualified UnitTests

main :: IO ()
main = do
    ioTests <- UnitTests.ioTests
    let allTests = DF.tests : Properties.tests : UnitTests.tests : ioTests
    defaultMain (testGroup "tests" allTests)

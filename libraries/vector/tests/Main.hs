module Main (main) where

import qualified Tests.Vector
import qualified Tests.Vector.UnitTests
import qualified Tests.Bundle
import qualified Tests.Move

import Test.Tasty (defaultMain,testGroup)

main :: IO ()
main = defaultMain $ testGroup "toplevel" $ Tests.Bundle.tests
                  ++ Tests.Vector.tests
                  ++ Tests.Vector.UnitTests.tests
                  ++ Tests.Move.tests


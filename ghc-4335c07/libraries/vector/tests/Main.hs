module Main (main) where

import qualified Tests.Vector
import qualified Tests.Bundle
import qualified Tests.Move

import Test.Framework (defaultMain)

main = defaultMain $ Tests.Bundle.tests
                  ++ Tests.Vector.tests
                  ++ Tests.Move.tests


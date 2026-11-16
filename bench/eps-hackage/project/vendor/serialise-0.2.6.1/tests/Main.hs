module Main
  ( main -- :: IO ()
  ) where
import           Test.Tasty (defaultMain, testGroup)

import qualified Tests.IO        as IO
import qualified Tests.Regress   as Regress
import qualified Tests.Serialise as Serialise
import qualified Tests.Negative  as Negative
import qualified Tests.Deriving  as Deriving
import qualified Tests.GeneralisedUTF8  as GeneralisedUTF8

main :: IO ()
main =
  defaultMain $
  testGroup "CBOR tests"
    [ Serialise.testTree
    , Serialise.testGenerics
    , Negative.testTree
    , IO.testTree
    , Regress.testTree
    , Deriving.testTree
    , GeneralisedUTF8.testTree
    ]

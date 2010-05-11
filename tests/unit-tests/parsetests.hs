module Main (main) where

import Test.HUnit
import RdrName (RdrName)
import DynFlags (defaultDynFlags)
import Haddock.Lex (tokenise)
import Haddock.Parse (parseParas)
import Haddock.Types

instance Show RdrName where
  show x = "RdrName"

data ParseTest = ParseTest {
    input   :: String
  , result  :: (Maybe (Doc RdrName))
  }

tests :: [ParseTest]
tests = [
    ParseTest {
      input  = "foobar"
    , result = Just $ DocParagraph $ DocString "foobar\n"
    }

  , ParseTest {
      input  = "foobar\n\nghci> fib 10\n55"
    , result = Just $ DocAppend (DocParagraph $ DocString "foobar\n") (DocExamples $ [Example "fib 10" ["55"]])
    }

  , ParseTest {
      input  = "foobar\nghci> fib 10\n55"
    , result = Nothing -- parse error
    }

  , ParseTest {
      input  = "foobar\n\n> some code"
    , result = Just (DocAppend (DocParagraph (DocString "foobar\n")) (DocCodeBlock (DocString " some code\n")))
    }

  , ParseTest {
      input  = "foobar\n> some code"
    , result = Nothing -- parse error
    }
  ]


main = do
  _ <- runTestTT $ TestList $ map toTestCase tests
  return ();
  where

    toTestCase :: ParseTest -> Test
    toTestCase (ParseTest input result) = TestCase $ assertEqual input (parse input) result

    parse :: String -> Maybe (Doc RdrName)
    parse input = parseParas $ tokenise defaultDynFlags input (0,0)

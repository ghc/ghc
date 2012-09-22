{-# LANGUAGE StandaloneDeriving, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Test.HUnit
import RdrName (RdrName)
import DynFlags (defaultDynFlags)
import Haddock.Lex (tokenise)
import Haddock.Parse (parseParas)
import Haddock.Types
import Outputable
import Data.Monoid

dynFlags = defaultDynFlags (error "dynFlags for Haddock tests: undefined")

instance Outputable a => Show a where
  show = showSDoc dynFlags . ppr

deriving instance Show a => Show (Doc a)
deriving instance Eq a =>Eq (Doc a)

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
      input  = "foobar\n\n>>> fib 10\n55"
    , result = Just $ DocAppend (DocParagraph $ DocString "foobar\n") (DocExamples $ [Example "fib 10" ["55"]])
    }

  , ParseTest {
      input  = "foobar\n>>> fib 10\n55"
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

  -- test <BLANKLINE> support
  , ParseTest {
      input  = ">>> putFooBar\nfoo\n<BLANKLINE>\nbar"
    , result = Just $ DocExamples $ [Example "putFooBar" ["foo","","bar"]]
    }

  -- tests for links
  , ParseTest {
      input  = "<http://example.com/>"
    , result = Just . DocParagraph $ hyperlink "http://example.com/" Nothing `mappend` DocString "\n"
    }

  , ParseTest {
      input  = "<http://example.com/ some link>"
    , result = Just . DocParagraph $ hyperlink "http://example.com/" (Just "some link") `mappend` DocString "\n"
    }
  ]

hyperlink :: String -> Maybe String -> Doc RdrName
hyperlink url = DocHyperlink . Hyperlink url

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ map toTestCase tests
  return ();
  where

    toTestCase :: ParseTest -> Test
    toTestCase (ParseTest s r) = TestCase $ assertEqual s r (parse s)

    parse :: String -> Maybe (Doc RdrName)
    parse s = parseParas $ tokenise dynFlags s (0,0)

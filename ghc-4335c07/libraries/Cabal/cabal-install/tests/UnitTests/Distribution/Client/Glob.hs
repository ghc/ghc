{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.Glob (tests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Char
import Data.List
import Distribution.Text (display, parse, simpleParse)
import Distribution.Compat.ReadP

import Distribution.Client.Glob
import UnitTests.Distribution.Client.ArbitraryInstances

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Exception


tests :: [TestTree]
tests =
  [ testProperty "print/parse roundtrip" prop_roundtrip_printparse
  , testCase     "parse examples"        testParseCases
  ]

--TODO: [nice to have] tests for trivial globs, tests for matching,
-- tests for windows style file paths

prop_roundtrip_printparse :: FilePathGlob -> Bool
prop_roundtrip_printparse pathglob =
  -- can't use simpleParse because it mis-handles trailing spaces
  case [ x | (x, []) <- readP_to_S parse (display pathglob) ] of
    xs@(_:_) -> last xs == pathglob
    _        -> False

-- first run, where we don't even call updateMonitor
testParseCases :: Assertion
testParseCases = do

  FilePathGlob (FilePathRoot "/") GlobDirTrailing <- testparse "/"
  FilePathGlob FilePathHomeDir  GlobDirTrailing <- testparse "~/"

  FilePathGlob (FilePathRoot "A:\\") GlobDirTrailing <- testparse "A:/"
  FilePathGlob (FilePathRoot "Z:\\") GlobDirTrailing <- testparse "z:/"
  FilePathGlob (FilePathRoot "C:\\") GlobDirTrailing <- testparse "C:\\"
  FilePathGlob FilePathRelative (GlobFile [Literal "_:"]) <- testparse "_:"

  FilePathGlob FilePathRelative
    (GlobFile [Literal "."]) <- testparse "."

  FilePathGlob FilePathRelative
    (GlobFile [Literal "~"]) <- testparse "~"

  FilePathGlob FilePathRelative
    (GlobDir  [Literal "."] GlobDirTrailing) <- testparse "./"

  FilePathGlob FilePathRelative
    (GlobFile [Literal "foo"]) <- testparse "foo"

  FilePathGlob FilePathRelative
    (GlobDir [Literal "foo"]
      (GlobFile [Literal "bar"])) <- testparse "foo/bar"

  FilePathGlob FilePathRelative
    (GlobDir [Literal "foo"]
      (GlobDir [Literal "bar"] GlobDirTrailing)) <- testparse "foo/bar/"

  FilePathGlob (FilePathRoot "/")
    (GlobDir [Literal "foo"]
      (GlobDir [Literal "bar"] GlobDirTrailing)) <- testparse "/foo/bar/"

  FilePathGlob FilePathRelative
    (GlobFile [WildCard]) <- testparse "*"

  FilePathGlob FilePathRelative
    (GlobFile [WildCard,WildCard]) <- testparse "**" -- not helpful but valid

  FilePathGlob FilePathRelative
    (GlobFile [WildCard, Literal "foo", WildCard]) <- testparse "*foo*"

  FilePathGlob FilePathRelative
    (GlobFile [Literal "foo", WildCard, Literal "bar"]) <- testparse "foo*bar"

  FilePathGlob FilePathRelative
    (GlobFile [Union [[WildCard], [Literal "foo"]]]) <- testparse "{*,foo}"

  parseFail "{"
  parseFail "}"
  parseFail ","
  parseFail "{"
  parseFail "{{}"
  parseFail "{}"
  parseFail "{,}"
  parseFail "{foo,}"
  parseFail "{,foo}"

  return ()

testparse :: String -> IO FilePathGlob
testparse s =
    case simpleParse s of
      Just p  -> return p
      Nothing -> throwIO $ HUnitFailure ("expected parse of: " ++ s)

parseFail :: String -> Assertion
parseFail s =
    case simpleParse s :: Maybe FilePathGlob of
      Just _  -> throwIO $ HUnitFailure ("expected no parse of: " ++ s)
      Nothing -> return ()

instance Arbitrary FilePathGlob where
  arbitrary = (FilePathGlob <$> arbitrary <*> arbitrary)
                `suchThat` validFilePathGlob

  shrink (FilePathGlob root pathglob) =
    [ FilePathGlob root' pathglob'
    | (root', pathglob') <- shrink (root, pathglob)
    , validFilePathGlob (FilePathGlob root' pathglob') ]

validFilePathGlob :: FilePathGlob -> Bool
validFilePathGlob (FilePathGlob FilePathRelative pathglob) =
  case pathglob of
    GlobDirTrailing             -> False
    GlobDir [Literal "~"] _     -> False
    GlobDir [Literal (d:":")] _ 
      | isLetter d              -> False
    _                           -> True
validFilePathGlob _ = True

instance Arbitrary FilePathRoot where
  arbitrary =
    frequency
      [ (3, pure FilePathRelative)
      , (1, pure (FilePathRoot unixroot))
      , (1, FilePathRoot <$> windrive)
      , (1, pure FilePathHomeDir)
      ]
    where
      unixroot = "/"
      windrive = do d <- choose ('A', 'Z'); return (d : ":\\")

  shrink FilePathRelative     = []
  shrink (FilePathRoot _)     = [FilePathRelative]
  shrink FilePathHomeDir      = [FilePathRelative]


instance Arbitrary FilePathGlobRel where
  arbitrary = sized $ \sz ->
    oneof $ take (max 1 sz)
      [ pure GlobDirTrailing
      , GlobFile  <$> (getGlobPieces <$> arbitrary)
      , GlobDir   <$> (getGlobPieces <$> arbitrary)
                  <*> resize (sz `div` 2) arbitrary
      ]

  shrink GlobDirTrailing = []
  shrink (GlobFile glob) =
      GlobDirTrailing
    : [ GlobFile (getGlobPieces glob') | glob' <- shrink (GlobPieces glob) ]
  shrink (GlobDir glob pathglob) =
      pathglob
    : GlobFile glob
    : [ GlobDir (getGlobPieces glob') pathglob'
      | (glob', pathglob') <- shrink (GlobPieces glob, pathglob) ]

newtype GlobPieces = GlobPieces { getGlobPieces :: [GlobPiece] }
  deriving Eq

instance Arbitrary GlobPieces where
  arbitrary = GlobPieces . mergeLiterals <$> shortListOf1 5 arbitrary

  shrink (GlobPieces glob) =
    [ GlobPieces (mergeLiterals (getNonEmpty glob'))
    | glob' <- shrink (NonEmpty glob) ]

mergeLiterals :: [GlobPiece] -> [GlobPiece]
mergeLiterals (Literal a : Literal b : ps) = mergeLiterals (Literal (a++b) : ps)
mergeLiterals (Union as : ps) = Union (map mergeLiterals as) : mergeLiterals ps
mergeLiterals (p:ps) = p : mergeLiterals ps
mergeLiterals []     = []

instance Arbitrary GlobPiece where
  arbitrary = sized $ \sz ->
    frequency
      [ (3, Literal <$> shortListOf1 10 (elements globLiteralChars))
      , (1, pure WildCard)
      , (1, Union <$> resize (sz `div` 2) (shortListOf1 5 (shortListOf1 5 arbitrary)))
      ]

  shrink (Literal str) = [ Literal str'
                         | str' <- shrink str
                         , not (null str')
                         , all (`elem` globLiteralChars) str' ]
  shrink WildCard       = []
  shrink (Union as)     = [ Union (map getGlobPieces (getNonEmpty as'))
                          | as' <- shrink (NonEmpty (map GlobPieces as)) ]

globLiteralChars :: [Char]
globLiteralChars = ['\0'..'\128'] \\ "*{},/\\"


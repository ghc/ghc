{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Text.Blaze.Tests
    ( tests
    ) where

import Prelude hiding (div, id, null)
import Data.Monoid (mempty, mappend)
import Control.Monad (replicateM)
import Control.Applicative (Applicative (..), (<$>))
import Data.Word (Word8)
import Data.Char (ord, isControl)
import qualified Data.List as List
import qualified Prelude as Prelude

import Test.Tasty (TestTree)
import Test.HUnit (Assertion, (@=?))
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC

import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Tests.Util

tests :: [TestTree]
tests = [ testProperty "left identity Monoid law"  monoidLeftIdentity
        , testProperty "right identity Monoid law" monoidRightIdentity
        , testProperty "associativity Monoid law"  monoidAssociativity
        , testProperty "mconcat Monoid law"        monoidConcat
        , testProperty "identity Applicative law"  applicativeIdentity
        , testProperty "post escaping characters"  postEscapingCharacters
        , testProperty "valid UTF-8"               isValidUtf8
        , testProperty "external </ sequence"      externalEndSequence
        , testProperty "well nested <>"            wellNestedBrackets
        , testProperty "unsafeByteString id"       unsafeByteStringId

        , testCase     "conditional attributes"    conditionalAttributes
        , testCase     "contents 1"                contents1
        , testCase     "empty 1"                   empty1
        , testCase     "empty 2"                   empty2
        , testCase     "comment 1"                 comment1
        ]

-- | The left identity Monoid law.
--
monoidLeftIdentity :: Markup -> Bool
monoidLeftIdentity h = (return () >> h) == h

-- | The right identity Monoid law.
--
monoidRightIdentity :: Markup -> Bool
monoidRightIdentity h = (h >> return ()) == h

-- | The associativity Monoid law.
--
monoidAssociativity :: Markup -> Markup -> Markup -> Bool
monoidAssociativity x y z = (x >> (y >> z)) == ((x >> y) >> z)

-- | Concatenation Monoid law.
--
monoidConcat :: [Markup] -> Bool
monoidConcat xs = sequence_ xs == foldr (>>) (return ()) xs

-- | Applicative identity law.
--
applicativeIdentity :: Markup -> Bool
applicativeIdentity x = (pure Prelude.id <*> x) == x

-- | Escaped content cannot contain certain characters.
--
postEscapingCharacters :: String -> Bool
postEscapingCharacters str =
    LB.all (`notElem` forbidden) $ renderUsingUtf8 (string str)
  where
    forbidden = map (fromIntegral . ord) "\"'<>"

-- | Check if the produced bytes are valid UTF-8
--
isValidUtf8 :: Markup -> Bool
isValidUtf8 = isValidUtf8' . LB.unpack . renderUsingUtf8
  where
    isIn x y z = (x <= z) && (z <= y)
    isValidUtf8' :: [Word8] -> Bool
    isValidUtf8' [] = True
    isValidUtf8' (x:t)
        -- One byte
        | isIn 0x00 0x7f x = isValidUtf8' t
        -- Two bytes
        | isIn 0xc0 0xdf x = case t of
            (y:t') -> isIn 0x80 0xbf y && isValidUtf8' t'
            _      -> False
        -- Three bytes
        | isIn 0xe0 0xef x = case t of
            (y:z:t') -> all (isIn 0x80 0xbf) [y, z] && isValidUtf8' t'
            _        -> False
        -- Four bytes
        | isIn 0xf0 0xf7 x = case t of
            (y:z:u:t') -> all (isIn 0x80 0xbf) [y, z, u] && isValidUtf8' t'
            _          -> False
        | otherwise = False

-- | Rendering an unsafe bytestring should not do anything
--
unsafeByteStringId :: [Word8] -> Bool
unsafeByteStringId ws =
    LB.pack ws == renderUsingUtf8 (unsafeByteString $ SB.pack ws)

-- | Check if the "</" sequence does not appear in @<script>@ or @<style>@ tags.
--
externalEndSequence :: String -> Bool
externalEndSequence = not . List.isInfixOf "</" . LBC.unpack
                    . renderUsingUtf8 . external . string

-- | Check that the "<>" characters are well-nested.
--
wellNestedBrackets :: Markup -> Bool
wellNestedBrackets = wellNested False . LBC.unpack . renderUsingUtf8
  where
    wellNested isOpen [] = not isOpen
    wellNested isOpen (x:xs) = case x of
        '<' -> if isOpen then False else wellNested True xs
        '>' -> if isOpen then wellNested False xs else False
        _   -> wellNested isOpen xs

conditionalAttributes :: Assertion
conditionalAttributes =
    "<p class=\"foo\">Hello</p><p id=\"2nd\">World</p>" @=? renderUsingUtf8 html
  where
    html = do
        p !? (4 > length [()], class_ "foo") $ "Hello"
        p !? (List.null [()], class_ "bar") !? (True, id "2nd") $ "World"

contents1 :: Assertion
contents1 = "Hello World!" @=? renderUsingUtf8 (contents html)
  where
    html :: Markup
    html = div $ do
        p ! id "para" $ "Hello "
        stringComment "Test test"
        img ! name "An image"
        p "World!"

empty1 :: Assertion
empty1 = True @=? null html
  where
    html :: Markup
    html = do
        ""
        ""
        mempty

empty2 :: Assertion
empty2 = False @=? null html
  where
    html :: Markup
    html = "" `mappend` "" `mappend` p "a"

comment1 :: Assertion
comment1 = preEscapedString "<div>Hello <!-- Test --> World!</div>" @=? html
  where
    html :: Markup
    html = div $ do
        "Hello "
        stringComment "Test"
        " World!"

-- Show instance for the HTML type, so we can debug.
--
instance Show Markup where
    show = show . renderUsingUtf8

-- Eq instance for the HTML type, so we can compare the results.
--
instance Eq Markup where
    x == y =  renderUsingString x == renderUsingString y
           && renderUsingText x   == renderUsingText y
           && renderUsingUtf8 x   == renderUsingUtf8 y
           -- Some cross-checks
           && renderUsingString x == renderUsingText y
           && renderUsingText x   == renderUsingUtf8 y

-- Arbitrary instance for the HTML type.
--
instance Arbitrary Markup where
    arbitrary = arbitraryMarkup 4

-- | Auxiliary function for the arbitrary instance of the HTML type, used
-- to limit the depth and size of the type.
--
arbitraryMarkup :: Int       -- ^ Maximum depth.
              -> Gen Markup  -- ^ Resulting arbitrary HTML snippet.
arbitraryMarkup depth = do
    -- Choose the size (width) of this element.
    size <- choose (0, 3)

    -- Generate `size` new HTML snippets.
    children <- replicateM size arbitraryChild

    -- Return a concatenation of these children.
    return $ sequence_ children
  where
    -- Generate an arbitrary child. Do not take a parent when we have no depth
    -- left, obviously.
    arbitraryChild = do
        child <- oneof $  [arbitraryLeaf, arbitraryString, return mempty]
                       ++ [arbitraryParent | depth > 0]

        -- Generate some attributes for the child.
        size <- choose (0, 4)
        attributes <- replicateM size arbitraryAttribute
        return $ foldl (!) child attributes

    -- Generate an arbitrary parent element.
    arbitraryParent = do
        parent <- elements [p, div, table]
        parent <$> arbitraryMarkup (depth - 1)

    -- Generate an arbitrary leaf element.
    arbitraryLeaf = oneof $ map return [img, br, area]

    -- Generate arbitrary string element.
    arbitraryString = do
        s <- genString
        return $ string s

    -- Generate an arbitrary HTML attribute.
    arbitraryAttribute = do
        attr <- elements [id, class_, name]
        value <- genString
        return $ attr $ stringValue value

    -- Don't use control characters
    genString = filter (not . isControl) <$> arbitrary

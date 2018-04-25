module UnitTests.Distribution.Utils.ShortText
    ( tests
    ) where

import Data.Monoid as Mon
import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.Compat.Binary (encode, decode)

import Distribution.Utils.ShortText

prop_ShortTextOrd :: String -> String -> Bool
prop_ShortTextOrd a b = compare a b == compare (toShortText a) (toShortText b)

prop_ShortTextMonoid :: String -> String -> Bool
prop_ShortTextMonoid a b = Mon.mappend a b == fromShortText (mappend (toShortText a) (toShortText b))

prop_ShortTextId :: String -> Bool
prop_ShortTextId a = (fromShortText . toShortText) a == a

prop_ShortTextBinaryId :: String -> Bool
prop_ShortTextBinaryId a = (decode . encode) a' == a'
  where
    a' = toShortText a

tests :: [TestTree]
tests =
    [ testProperty "ShortText Id" prop_ShortTextId
    , testProperty "ShortText Ord" prop_ShortTextOrd
    , testProperty "ShortText Monoid" prop_ShortTextMonoid
    , testProperty "ShortText BinaryId" prop_ShortTextBinaryId
    ]

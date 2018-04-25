module Test.TestUtil where

import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

assertFailure' :: String -> IO a
assertFailure' s = do
    assertFailure s
    return undefined

assertJust :: Maybe a -> IO a
assertJust (Just a) = return a
assertJust Nothing = assertFailure' "Nothing"

class NameTest a where
    nameTest :: String -> a -> TestTree

instance NameTest [TestTree] where
    nameTest = testGroup

instance NameTest Assertion where
    nameTest = testCase

instance NameTest Property where
    nameTest = testProperty

instance NameTest Result where
    nameTest name = nameTest name . property

instance (Arbitrary a,Show a,Testable b) => NameTest (a -> b) where
    nameTest name = nameTest name . property

tgroup :: (Show a,NameTest t) => [a] -> (a -> t) -> [TestTree]
tgroup aa f = fmap (\a -> nameTest (show a) $ f a) aa

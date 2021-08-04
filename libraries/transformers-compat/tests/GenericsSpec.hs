{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GenericsSpec (main, spec) where

import Data.Functor.Classes
import Data.Proxy (Proxy(..))

import GenericsTypes

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

import Text.Read (minPrec)

main :: IO ()
main = hspec spec

prop_Eq :: (Eq a, Eq (f a), Eq1 f) => f a -> f a -> Bool
prop_Eq x y = (x == y) == eq1 x y

eqSpec :: forall f a. (Arbitrary (f a), Show (f a),
                       Eq a, Eq (f a), Eq1 f)
       => Proxy (f a) -> Spec
eqSpec _ = prop "has a valid Eq1 instance" (prop_Eq :: f a -> f a -> Bool)

prop_Ord :: (Ord a, Ord (f a), Ord1 f) => f a -> f a -> Bool
prop_Ord x y = compare x y == compare1 x y

ordSpec :: forall f a. (Arbitrary (f a), Show (f a),
                        Ord a, Ord (f a), Ord1 f)
        => Proxy (f a) -> Spec
ordSpec _ = prop "has a valid Ord1 instance" (prop_Ord :: f a -> f a -> Bool)

-- Adapted from the definition of readEither
readEither' :: String -> (Int -> ReadS a) -> Either String a
readEither' s rs =
  case [ x | (x,"") <- rs minPrec s ] of
    [x] -> Right x
    []  -> Left "read': no parse"
    _   -> Left "read': ambiguous parse"

read' :: String -> (Int -> ReadS a) -> a
read' s = either error id . readEither' s

prop_Read :: forall f a. (Read a, Read (f a), Read1 f,
                          Eq (f a), Show (f a))
          => f a -> Bool
prop_Read x = readArb readsPrec == readArb readsPrec1
  where
    readArb :: (Int -> ReadS (f a)) -> f a
    readArb = read' (show x)

readSpec :: forall f a. (Arbitrary (f a), Eq (f a), Show (f a),
                         Read a, Read (f a), Read1 f)
         => Proxy (f a) -> Spec
readSpec _ = prop "has a valid Read1 instance" (prop_Read :: f a -> Bool)

prop_Show :: (Show a, Show (f a), Show1 f) => Int -> f a -> Bool
prop_Show p x = showsPrec p x "" == showsPrec1 p x ""

showSpec :: forall f a. (Arbitrary (f a), Show a, Show (f a), Show1 f)
         => Proxy (f a) -> Spec
showSpec _ = prop "has a valid Show1 instance" (prop_Show :: Int -> f a -> Bool)

classes1Spec :: forall f a. (Arbitrary (f a),
                             Ord  a, Ord  (f a), Ord1  f,
                             Read a, Read (f a), Read1 f,
                             Show a, Show (f a), Show1 f)
             => String -> Proxy (f a) -> Spec
classes1Spec str proxy =
    describe str $ do eqSpec proxy
                      ordSpec proxy
                      readSpec proxy
                      showSpec proxy

spec :: Spec
spec = parallel $ do
    classes1Spec "TestParam" (Proxy :: Proxy (TestParam Int))
    classes1Spec "T#"        (Proxy :: Proxy (T# Int))
    classes1Spec "Infix"     (Proxy :: Proxy (Infix Int))
    classes1Spec "GADT"      (Proxy :: Proxy (GADT Int))
    classes1Spec "Record"    (Proxy :: Proxy (Record Int))
    describe "Prim" $ do
        let proxy :: Proxy (Prim Int)
            proxy = Proxy
        eqSpec proxy
        ordSpec proxy
        showSpec proxy
    describe "Empty" $ do
        let proxy :: Proxy (Empty Int)
            proxy = Proxy
        eqSpec proxy
        ordSpec proxy
        it "should fail to parse eagerly" $ do
          let readEmpty :: String -> (Int -> ReadS (Empty Int)) -> Either String (Empty Int)
              readEmpty = readEither'
          readEmpty ""             readsPrec `shouldBe` readEmpty ""             readsPrec1
          readEmpty (error "boom") readsPrec `shouldBe` readEmpty (error "boom") readsPrec1

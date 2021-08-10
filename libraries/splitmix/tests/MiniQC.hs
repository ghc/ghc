{-# LANGUAGE DeriveFunctor #-}
-- | This QC doesn't shrink :(
module MiniQC where

import Control.Monad                  (ap)
import Data.Int                       (Int32, Int64)
import Data.Word                      (Word32, Word64)
import Prelude ()
import Prelude.Compat
import Test.Framework.Providers.API   (Test, TestName)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assertFailure)

import System.Random.SplitMix

newtype Gen a = Gen { unGen :: SMGen -> a }
  deriving (Functor)

instance Applicative Gen where
    pure x = Gen (const x)
    (<*>) = ap

instance Monad Gen where
    return = pure

    m >>= k = Gen $ \g ->
        let (g1, g2) = splitSMGen g
        in unGen (k (unGen m g1)) g2

class Arbitrary a where
    arbitrary :: Gen a

instance Arbitrary Word32 where
    arbitrary = Gen $ \g -> fst (nextWord32 g)
instance Arbitrary Word64 where
    arbitrary = Gen $ \g -> fst (nextWord64 g)
instance Arbitrary Int32 where
    arbitrary = Gen $ \g -> fromIntegral (fst (nextWord32 g))
instance Arbitrary Int64 where
    arbitrary = Gen $ \g -> fromIntegral (fst (nextWord64 g))
instance Arbitrary Double where
    arbitrary = Gen $ \g -> fst (nextDouble g)

newtype Property = Property { unProperty :: Gen ([String], Bool) }

class Testable a where
    property :: a -> Property

instance Testable Property where
    property = id

instance Testable Bool where
    property b = Property $ pure ([show b], b)

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
    property f = Property $ do
        x <- arbitrary
        (xs, b) <- unProperty (property (f x))
        return (show x : xs, b)

forAllBlind :: Testable prop => Gen a -> (a -> prop) -> Property
forAllBlind g f = Property $ do
    x <- g
    (xs, b) <- unProperty (property (f x))
    return ("<blind>" : xs, b)

counterexample :: Testable prop => String -> prop -> Property
counterexample msg prop = Property $ do
    (xs, b) <- unProperty (property prop)
    return (msg : xs, b)

testMiniProperty :: Testable prop => TestName -> prop -> Test
testMiniProperty name prop = testCase name $ do
    g <- newSMGen
    go (100 :: Int) g
  where
    go n _ | n <= 0  = return ()
    go n g           = do
        let (g1, g2) = splitSMGen g
        case unGen (unProperty (property prop)) g1 of
            (_, True) -> return ()
            (xs, False) -> assertFailure (unlines (reverse xs))
        go (pred n) g2

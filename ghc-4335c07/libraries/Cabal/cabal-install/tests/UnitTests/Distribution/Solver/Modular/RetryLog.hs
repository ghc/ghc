{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Distribution.Solver.Modular.RetryLog (
  tests
  ) where

import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.RetryLog
import Distribution.Solver.Types.Progress

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
         ( Arbitrary(..), Blind(..), listOf, oneof, testProperty, (===))

type Log a = Progress a String String

tests :: [TestTree]
tests = [
    testProperty "'toProgress . fromProgress' is identity" $ \p ->
        toProgress (fromProgress p) === (p :: Log Int)

  , testProperty "'mapFailure f' is like 'foldProgress Step (Fail . f) Done'" $
        let mapFailureProgress f = foldProgress Step (Fail . f) Done
        in \(Blind f) p ->
               toProgress (mapFailure f (fromProgress p))
               === mapFailureProgress (f :: String -> Int) (p :: Log Int)

  , testProperty "'retry p f' is like 'foldProgress Step f Done p'" $
      \p (Blind f) ->
        toProgress (retry (fromProgress p) (fromProgress . f))
        === (foldProgress Step f Done (p :: Log Int) :: Log Int)

  , testProperty "failWith" $ \step failure ->
        toProgress (failWith step failure)
        === (Step step (Fail failure) :: Log Int)

  , testProperty "succeedWith" $ \step success ->
        toProgress (succeedWith step success)
        === (Step step (Done success) :: Log Int)

  , testProperty "continueWith" $ \step p ->
        toProgress (continueWith step (fromProgress p))
        === (Step step p :: Log Int)

  , testCase "tryWith with failure" $
        let failure = Fail "Error"
            s = Step Success
        in toProgress (tryWith Success $ fromProgress (s (s failure)))
           @?= (s (Step Enter (s (s (Step Leave failure)))) :: Log Message)

  , testCase "tryWith with success" $
        let done = Done "Done"
            s = Step Success
        in toProgress (tryWith Success $ fromProgress (s (s done)))
           @?= (s (Step Enter (s (s done))) :: Log Message)
  ]

instance (Arbitrary step, Arbitrary fail, Arbitrary done)
    => Arbitrary (Progress step fail done) where
  arbitrary = do
    steps <- listOf arbitrary
    end <- oneof [Fail `fmap` arbitrary, Done `fmap` arbitrary]
    return $ foldr Step end steps

deriving instance (Eq step, Eq fail, Eq done) => Eq (Progress step fail done)

deriving instance (Show step, Show fail, Show done)
    => Show (Progress step fail done)

deriving instance Eq Message
deriving instance Show Message

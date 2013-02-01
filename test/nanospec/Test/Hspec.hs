{-# LANGUAGE DeriveDataTypeable, CPP #-}
-- | A lightweight implementation of a subset of Hspec's API.
module Test.Hspec (
-- * Types
  SpecM
, Spec

-- * Defining a spec
, describe
, context
, it

-- ** Setting expectations
, Expectation
, expect
, shouldBe
, shouldReturn

-- * Running a spec
, hspec
) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.List (intercalate)
import           Data.Typeable
import qualified Control.Exception as E
import           System.Exit

-- a writer monad
data SpecM a = SpecM a [SpecTree]

add :: SpecTree -> SpecM ()
add s = SpecM () [s]

instance Monad SpecM where
  return a             = SpecM a []
  SpecM a xs >>= f = case f a of
    SpecM b ys -> SpecM b (xs ++ ys)

data SpecTree = SpecGroup String Spec
              | SpecExample String (IO Result)

data Result = Success | Failure String
  deriving (Eq, Show)

type Spec = SpecM ()

describe :: String -> Spec -> Spec
describe label = add . SpecGroup label

context :: String -> Spec -> Spec
context = describe

it :: String -> Expectation -> Spec
it label = add . SpecExample label . evaluateExpectation

-- | Summary of a test run.
data Summary = Summary Int Int

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)

runSpec :: Spec -> IO Summary
runSpec = runForrest []
  where
    runForrest :: [String] -> Spec -> IO Summary
    runForrest labels (SpecM () xs) = mconcat <$> mapM (runTree labels) xs

    runTree :: [String] -> SpecTree -> IO Summary
    runTree labels spec = case spec of
      SpecExample label x -> do
        putStr $ "/" ++ (intercalate "/" . reverse) (label:labels) ++ "/ "
        r <- x
        case r of
          Success   -> do
            putStrLn "OK"
            return (Summary 1 0)
          Failure err -> do
            putStrLn "FAILED"
            putStrLn err
            return (Summary 1 1)
      SpecGroup label xs  -> do
        runForrest (label:labels) xs

hspec :: Spec -> IO ()
hspec spec = do
  Summary total failures <- runSpec spec
  putStrLn (show total ++ " example(s), " ++ show failures ++ " failure(s)")
  when (failures /= 0) exitFailure

type Expectation = IO ()

infix 1 `shouldBe`, `shouldReturn`

shouldBe :: (Show a, Eq a) => a -> a -> Expectation
actual `shouldBe` expected =
  expect ("expected: " ++ show expected ++ "\n but got: " ++ show actual) (actual == expected)

shouldReturn :: (Show a, Eq a) => IO a -> a -> Expectation
action `shouldReturn` expected = action >>= (`shouldBe` expected)

expect :: String -> Bool -> Expectation
expect label f
  | f         = return ()
  | otherwise = E.throwIO (ExpectationFailure label)

data ExpectationFailure = ExpectationFailure String
  deriving (Show, Eq, Typeable)

instance E.Exception ExpectationFailure

evaluateExpectation :: Expectation -> IO Result
evaluateExpectation action = (action >> return Success)
  `E.catches` [
  -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT
  -- (ctrl-c).  All AsyncExceptions are re-thrown (not just UserInterrupt)
  -- because all of them indicate severe conditions and should not occur during
  -- normal operation.
    E.Handler $ \e -> E.throw (e :: E.AsyncException)

  , E.Handler $ \(ExpectationFailure err) -> return (Failure err)
  , E.Handler $ \e -> (return . Failure) ("*** Exception: " ++ show (e :: E.SomeException))
  ]

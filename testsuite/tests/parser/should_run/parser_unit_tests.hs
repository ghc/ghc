import GHC.Data.StringBuffer (stringToStringBuffer)
import qualified GHC.Data.StringBuffer as StringBuffer (StringBuffer (..))
import GHC.Parser.String (lexString, lexMultilineString)

import qualified Control.Exception as E
import Control.Monad (forM_, unless)

main :: IO ()
main = do
  forM_ tests $ \(testName, test) -> do
    result <- E.try test
    case result of
      Right () -> pure ()
      Left (e :: E.SomeException)
        | Just e' <- E.asyncExceptionFromException e -> do
            E.throwIO (e' :: E.AsyncException)
        | otherwise -> do
            putStrLn $ ">>> FAIL: " ++ testName
            putStrLn $ E.displayException e

{----- Test infrastructure -----}

data TestFailure = TestFailure String
  deriving (Show)

instance E.Exception TestFailure where
  displayException (TestFailure msg) = "Test failure:\n" ++ msg

testFailure :: String -> IO a
testFailure = E.throwIO . TestFailure

shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe actual expected =
  unless (actual == expected) $
    testFailure $
      "Got:      " ++ show actual ++ "\n" ++
      "Expected: " ++ show expected

type TestCase = (String, IO ())

testCase :: String -> IO () -> TestCase
testCase = (,)

{----- Tests -----}

tests :: [TestCase]
tests = concat
  [ stringTests
  ]

-- | Unit tests for GHC.Parser.String
stringTests :: [TestCase]
stringTests = concat
  [ escapedBackslashTests
  ]
  where
    withBuf f s = let buf = stringToStringBuffer s in f (StringBuffer.len buf) buf

    -- Test all situations where backslashes can appear in escape characters (#25937)
    escapedBackslashTests =
      [ testCase label $ do
          withBuf lexStr input `shouldBe` Right output
      | (lexLabel, lexStr) <- [("strings", lexString), ("multiline strings", lexMultilineString)]
      , (label, input, output) <-
          [ ( "escaped backslashes in " ++ lexLabel ++ " not mistaken for string gaps"
            , [' ', '\\', '\\', ' ', '\\', '\\', ' ']
            , " \\ \\ "
            )
          , ( "escaped \\FS in " ++ lexLabel ++ " not mistaken for beginning of string gap"
            , ['\\', '^', '\\']
            , "\FS"
            )
          , ( "escaped \\FS in " ++ lexLabel ++ " not mistaken for unterminated string gap"
            , ['\\', '^', '\\', ' ']
            , "\FS "
            )
          , ( "escaped \\FS in " ++ lexLabel ++ " does not collapse mistaken string gap"
            , ['\\', '^', '\\', ' ', '\\', 'n']
            , "\FS \n"
            )
          ]
      ]

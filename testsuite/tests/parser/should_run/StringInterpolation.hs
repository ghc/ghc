{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StringInterpolation #-}

main :: IO ()
main = mapM_ runTest allTests

data TestCase =
  forall a. Show a =>
  TestCase
    { label      :: String
    , expression :: a
    }

runTest :: TestCase -> IO ()
runTest TestCase{..} = do
  putStrLn $ "****************************************"
  putStrLn $ "Input:"
  putStr   $ unlines . map ("    " ++) . lines $ label
  putStrLn $ "====>"
  putStrLn $ "    " ++ show expression

allTests :: [TestCase]
allTests =
  [ TestCase -- empty interpolated string
      { label =
          """
          s""
          """
      , expression =
          s""
      }
  , TestCase -- interpolated string with no interpolations
      { label =
          """
          s"test"
          """
      , expression =
          s"test"
      }
  , TestCase -- disambiguate s as a variable and s as a delimiter
      { label =
          """
          let s = "world" in s ++ s" hello ${s}"
          """
      , expression =
          let s = "world" in s ++ s" hello ${s}"
      }
  , TestCase -- curly brace Char
      { label =
          """
          s"a ${'}'} b"
          """
      , expression =
          s"a ${'}'} b"
      }
  , TestCase -- interpolated expression with comment
      { label =
          """
          let x = "test" in s"a ${x {- inline comment -}} b"
          """
      , expression =
          let x = "test" in s"a ${x {- inline comment -}} b"
      }
  , TestCase -- nested interpolated strings within interpolated strings
      { label =
          """
          s"a ${"asdf" ++ s" b ${1 + 2 :: Int}"}"
          """
      , expression =
          s"a ${"asdf" ++ s" b ${1 + 2 :: Int}"}"
      }
  , TestCase -- interpolate with only expressions
      { label =
          """
          let s = "test" in s"${s}${s}"
          """
      , expression =
          let s = "test" in s"${s}${s}"
      }
  , TestCase -- interpolate only whitespace
      { label =
          """
          s"    "
          """
      , expression =
          s"    "
      }
  , TestCase -- interpolate with only expressions + whitespace
      { label =
          """
          let s = "test" in s"${s}   ${s}"
          """
      , expression =
          let s = "test" in s"${s}   ${s}"
      }
  , TestCase -- interpolate comments
      { label =
          """
          show [s"{- test -}", s"-- asdf", s"{-", s"--"]
          """
      , expression =
          [s"{- test -}", s"-- asdf", s"{-", s"--"]
      }
  , TestCase -- type inference works with deep nesting without overloaded strings
      { label =
          """
          let s0 = "0"
              s1 = s"${s0} ${s0}"
              s2 = s"${s1} ${s0}"
              s3 = s"${s2} ${s0}"
          in s3
          """
      , expression =
          let s0 = "0"
              s1 = s"${s0} ${s0}"
              s2 = s"${s1} ${s0}"
              s3 = s"${s2} ${s0}"
          in s3
      }
  , TestCase -- interpolating a record constructor
      { label =
          """
          s"Owner: ${show Person{name = "Alice"}}"
          """
      , expression =
          s"Owner: ${show Person{name = "Alice"}}"
      }
  , TestCase -- string gaps + escapes
      { label =
          """
          let x = "test" in s"a \\ESC \\
            \\ ${x} \\n\\
            \\ b \\t"
          """
      , expression =
          let x = "test" in s"a \ESC \
            \ ${x} \n\
            \ b \t"
      }
  -- TODO(bchinn): interpolated multiline strings
  ]

{----- Types for tests -----}

data Person = Person { name :: String } deriving (Show)

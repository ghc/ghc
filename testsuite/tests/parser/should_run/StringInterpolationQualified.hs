{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedLiterals #-}
{-# LANGUAGE RecordWildCards #-}

import qualified StringInterpolationQualified_SQL as SQL

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
  [ TestCase -- custom interpolation implementations
      { label =
          """
          let
            name = "'Robert'; DROP TABLE Students;--"
            age = 10 :: Int
          in
            SQL."SELECT * FROM tab WHERE name ILIKE ${name} and age = ${age}"
          """
      , expression =
          let
            name = "'Robert'; DROP TABLE Students;--"
            age = 10 :: Int
          in
            SQL."SELECT * FROM tab WHERE name ILIKE ${name} and age = ${age}"
      }
  -- TODO(bchinn): qualified interpolated multiline string
  ]

{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StringInterpolation #-}

import Data.String.Interpolate.Experimental
import Data.Text (Text)

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
  [ TestCase -- Text
      { label =
          """
          let s = "world"; x = True in s <> s" hello ${s} ${x}" :: Text
          """
      , expression =
          let s = "world"; x = True in s <> s" hello ${s} ${x}" :: Text
      }
  -- FIXME(bchinn): overloaded interpolated multiline string
  ]

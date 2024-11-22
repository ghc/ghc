{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StringInterpolation #-}

import Data.String.Experimental
import Data.Text (Text)
import Data.Text qualified as Text

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
          let s = "world" :: Text; x = True in s <> s" hello ${s} ${x}" :: Text
          """
      , expression =
          let s = "world" :: Text; x = True in s <> s" hello ${s} ${x}" :: Text
      }
  , TestCase -- Text (multiline)
      { label =
          """
          let s = "world" :: Text; x = True in
            s\"""
            hello
            ${s}
            ${x}
            \"""
          """
      , expression =
          let s = "world" :: Text; x = True in
            s"""
            hello
            ${s}
            ${x}
            """
      }
  ]

-- Remove when text provides this instance
instance Interpolate Text where
  interpolate = interpolate . Text.unpack

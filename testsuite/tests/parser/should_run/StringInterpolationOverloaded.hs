{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StringInterpolation #-}
{-# LANGUAGE TypeFamilies #-}

import Data.String.Interpolate.Experimental
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Builder qualified as LazyText
import Data.Text.Lazy.Builder.Int qualified as LazyText
import Data.Text.Lazy.Builder.RealFloat qualified as LazyText
import Data.Text.Lazy qualified as LazyText

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
  -- FIXME(bchinn): overloaded interpolated multiline string
  ]

-- Remove when text provides this instance
instance Interpolate Text where
  interpolate = interpolate . Text.unpack

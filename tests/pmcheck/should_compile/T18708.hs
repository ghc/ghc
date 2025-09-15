{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RebindableSyntax         #-}

module A (main) where

import           Prelude
import           Data.Text


fromString :: String -> Text
fromString = pack

y :: Text
y = "y"

main :: IO ()
main =  do
  case y of
    "y" -> return ()
  return ()

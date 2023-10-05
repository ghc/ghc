{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}

module T15645 where

import Prelude hiding (fail)

foo x = do
    Just y <- x
    return y

newtype Text = Text String

fail :: Text -> a
fail (Text x) = error x

fromString :: String -> Text
fromString = Text

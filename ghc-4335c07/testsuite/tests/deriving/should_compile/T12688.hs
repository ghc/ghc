{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module T12688 where

import Prelude (String,Show(..))

newtype Text = Text String

fromString :: String -> Text
fromString = Text

x :: Text
x = "x"

newtype Foo = Foo ()
  deriving (Show)

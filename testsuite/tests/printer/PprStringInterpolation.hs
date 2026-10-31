{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedStrings #-}
{-# LANGUAGE StringInterpolation #-}

module PprQualifiedStrings where

import qualified Example.Text as Text

exprs :: IO ()
exprs = do
  let name = "Alice"
  print s"My name is: ${name}. Nice to meet you!"
  print s"""
    My name is:
      ${name}
    Nice to meet you!
  """
  print Text.s"My name is: ${name}. Nice to meet you!"
  print Text.s"""
    My name is:
      ${name}
    Nice to meet you!
  """

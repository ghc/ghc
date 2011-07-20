-- !!! Deriving Show/Read for type with labelled fields.
--     (based on a Hugs bug report.)
module Main(main) where

data Options = 
   Options { s :: OptionKind } 
   deriving (Show, Read)

data OptionKind = 
   SpecialOptions { test :: Int }
   deriving (Show, Read)

x = Options{s=SpecialOptions{test=42}}

main = do
  print x
  print ((read (show x))::Options)

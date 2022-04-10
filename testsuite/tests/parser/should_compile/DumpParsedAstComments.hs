{-# LANGUAGE Haskell2010 #-}
{-
  Block comment at the beginning
  -}
module DumpParsedAstComments where

foo = do
  -- normal comment
  1

-- | Haddock comment
main = putStrLn "hello"

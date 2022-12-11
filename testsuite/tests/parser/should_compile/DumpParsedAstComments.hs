{-# LANGUAGE Haskell2010 #-}
{-
  Block comment at the beginning
  -}
module DumpParsedAstComments where

-- Other comment

-- comment 1 for foo
-- comment 2 for foo
foo = do
  -- normal comment
  1

-- | Haddock comment
main = putStrLn "hello"

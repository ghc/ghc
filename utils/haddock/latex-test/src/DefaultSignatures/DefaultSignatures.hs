{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DefaultSignatures #-}

module DefaultSignatures where

-- | Documentation for Foo.
class Foo a where
  -- | Documentation for bar and baz.
  bar, baz :: a -> String

  -- | Documentation for the default signature of bar.
  default bar :: Show a => a -> String
  bar = show

  -- | Documentation for baz'.
  baz' :: String -> a

  -- | Documentation for the default signature of baz'.
  default baz' :: Read a => String -> a
  baz' = read

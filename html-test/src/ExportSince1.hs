module ExportSince1
  ( hello1
  , hello2
  , HelloTy(..)
  , HelloClass(..)
  ) where

-- | @since 1.0
hello1 :: String
hello1 = "Hello"

-- | @since 1.0
hello2 :: String
hello2 = "Hello"

-- | @since 1.0
data HelloTy = HelloTy { field1 :: String }

-- | @since 1.0
class HelloClass a where
  method1 :: a -> String

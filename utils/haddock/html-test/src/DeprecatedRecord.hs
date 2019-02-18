module DeprecatedRecord where

-- | type Foo
data Foo = Foo {
  fooName  :: String -- ^ some name
, fooValue :: Int    -- ^ some value
}

{-# DEPRECATED fooValue "do not use this" #-}

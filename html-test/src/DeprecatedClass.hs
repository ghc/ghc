module DeprecatedClass where

-- | some class
class SomeClass a where
  -- | documentation for foo
  foo :: a -> a

{-# DEPRECATED SomeClass "SomeClass" #-}
{-# DEPRECATED foo "foo" #-}

class SomeOtherClass a where
  bar :: a -> a

{-# DEPRECATED SomeOtherClass "SomeOtherClass" #-}
{-# DEPRECATED bar "bar" #-}

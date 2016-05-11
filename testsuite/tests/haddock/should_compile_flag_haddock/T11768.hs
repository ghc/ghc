{-# LANGUAGE StandaloneDeriving #-}
module T11768 where

data Foo = Foo
  deriving Eq -- ^ Documenting a single type

data Bar = Bar
  deriving ( Eq -- ^ Documenting one of multiple types
           , Ord
           )

-- | Documenting a standalone deriving instance
deriving instance Read Bar

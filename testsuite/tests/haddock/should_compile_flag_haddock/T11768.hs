{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module T11768 where

class C a b

data Foo = Foo
  deriving Eq -- ^ Documenting a single type

data Bar = Bar
  deriving ( Eq -- ^ Documenting one of multiple types
           , Ord
           )
  deriving anyclass ( forall a. C a {- ^ Documenting forall type -} )

-- | Documenting a standalone deriving instance
deriving instance Read Bar

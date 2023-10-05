{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module T11768 where

class C a b

class D a

newtype DWrapper a = DWrap a
instance D (DWrapper a)

data Foo = Foo
  deriving Eq -- ^ Documenting a single type

data Bar = Bar
  deriving ( Eq -- ^ Documenting one of multiple types
           , Ord
           )
  deriving anyclass ( forall a. C a {- ^ Documenting forall type -} )
  deriving D {- ^ Documenting deriving via -} via DWrapper Bar

-- | Documenting a standalone deriving instance
deriving instance Read Bar

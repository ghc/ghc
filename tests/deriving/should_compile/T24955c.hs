{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}

module T24955c () where

data Foo a = Foo a

deriving instance Eq a => Eq (Foo a)

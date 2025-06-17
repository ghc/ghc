{-# LANGUAGE StandaloneDeriving, GHC2021 #-}

{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}

module T15798b () where

data Foo a = Foo a

deriving instance Eq a => Eq (Foo a)

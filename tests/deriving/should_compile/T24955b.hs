{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}
{-# LANGUAGE FunctionalDependencies #-}

module T24955b () where

class C a b | b -> a

instance C Int Int

newtype N = N Int
  deriving (C Int)

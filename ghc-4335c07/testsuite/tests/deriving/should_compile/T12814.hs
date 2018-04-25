{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module T12814 where

class C a where
  type T a

newtype Identity a = Identity a
  deriving C

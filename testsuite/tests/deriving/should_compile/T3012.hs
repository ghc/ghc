{-# LANGUAGE GADTs, StandaloneDeriving #-}

module T3012 where

data T a where
    Foo :: T Int
    Bar :: T Bool

deriving instance Show (T a)


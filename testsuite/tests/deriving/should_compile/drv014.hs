{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module ShouldCompile where

data T a  = T1 a | T2
newtype N = MkN Int

deriving instance Eq a => Eq (T a)
deriving instance Num N
deriving instance Eq N
deriving instance Show N

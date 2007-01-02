module ShouldCompile where

data T a  = T1 a | T2
newtype N = MkN Int

derive instance Eq (T a)
derive instance Num N
derive instance Eq N
derive instance Show N

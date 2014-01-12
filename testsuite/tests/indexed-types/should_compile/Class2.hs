{-# LANGUAGE TypeFamilies #-}

module Class2 where

data family T a
data instance T Int = TInt Int

data U = U (T Int)

instance Show a => Show (T a) where
  showsPrec k t = showString "T"

instance Show U where
  showsPrec k (U x) = showsPrec k x


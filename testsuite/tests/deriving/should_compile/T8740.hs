{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module T8740 where

data Abstract
data Reified
data Player

data Elect p a where
    ElectRefAsTypeOf :: Int -> Elect Abstract a -> Elect Abstract a
    ElectHandle :: a -> Elect Reified a
    Controller :: Elect Abstract Player
    Owner :: Elect Abstract Player
    You :: Elect Abstract Player

deriving instance (Eq a) => Eq (Elect p a)
deriving instance (Ord a) => Ord (Elect p a)

{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module T1496 where

data family Z :: * -> *

newtype instance Z Int = ZI Double
newtype instance Z Moo = ZM (Int,Int)

newtype Moo = Moo Int deriving(IsInt)
class IsInt t where
    isInt :: c Int -> c t

instance IsInt Int where isInt = id

main = case isInt (ZI 4.0) of ZM tu -> print tu
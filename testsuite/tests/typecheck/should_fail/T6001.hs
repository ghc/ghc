{-# LANGUAGE InstanceSigs #-}

module T6001 where

data DayKind = Work | Rest

instance Num DayKind where
  fromInteger :: Int -> DayKind
  fromInteger = undefined

{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE GADTs, DataKinds, TypeFamilies, BangPatterns #-}

module T18533 where

data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

type family Fam (b :: Bool)
type instance Fam 'True = T

data T = T Bool

f :: Fam b -> SBool b -> Bool
f !t s = case s of
    STrue -> a where a = case t of T a -> a
    _     -> False


g :: Bool -> Bool
g x = case x of
  True -> a where a = case x of True -> False
  False -> True

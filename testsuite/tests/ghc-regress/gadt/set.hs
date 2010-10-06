{-# LANGUAGE GADTs #-}

-- Provoked by 
-- http://www.haskell.org/pipermail/haskell-cafe/2007-January/021086.html

module ShouldCompile where

import Data.Set as Set

data Teq a b where Teq :: Teq a a

---------------------
data SetM1 a where
     SM1 :: Ord w => Teq w a -> Set.Set w -> SetM1 a

unionA1 :: SetM1 a -> SetM1 a -> SetM1 a
unionA1 (SM1 Teq m1) (SM1 Teq m2)
  = SM1 Teq (m1 `Set.union` m2)

unionB1 :: SetM1 a -> SetM1 a -> SetM1 a
unionB1 (SM1 p1 m1) (SM1 p2 m2)
  = case p1 of Teq -> case p2 of Teq -> SM1 Teq (m1 `Set.union` m2)

unionC1 :: SetM1 a -> SetM1 a -> SetM1 a
unionC1 (SM1 p1 m1) (SM1 p2 m2)
  = case (p1,p2) of (Teq,Teq) -> SM1 Teq (m1 `Set.union` m2)


---------------------
data SetM2 a where
     SM2 :: Ord w => Teq a w -> Set.Set w -> SetM2 a
	-- Different order of args in Teq

unionA2 :: SetM2 a -> SetM2 a -> SetM2 a
unionA2 (SM2 Teq m1) (SM2 Teq m2)
  = SM2 Teq (m1 `Set.union` m2)

unionB2 :: SetM2 a -> SetM2 a -> SetM2 a
unionB2 (SM2 p1 m1) (SM2 p2 m2)
  = case p1 of Teq -> case p2 of Teq -> SM2 Teq (m1 `Set.union` m2)

unionC2 :: SetM2 a -> SetM2 a -> SetM2 a
unionC2 (SM2 p1 m1) (SM2 p2 m2) 
  = case (p1,p2) of (Teq,Teq) -> SM2 Teq (m1 `Set.union` m2)


-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# OPTIONS_GHC -O2 -fforce-recomp #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import GHC.Base

class (Ord a) => Ix a where
    range :: (a,a) -> [a]

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD, Ix aE, Ix aF) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD,aE,aF)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) | i1 <- range (l1,u1),
                                                        i2 <- range (l2,u2),
                                                        i3 <- range (l3,u3),
                                                        i4 <- range (l4,u4),
                                                        i5 <- range (l5,u5),
                                                        i6 <- range (l6,u6),
                                                        i7 <- range (l7,u7),
                                                        i8 <- range (l8,u8),
                                                        i9 <- range (l9,u9),
                                                        iA <- range (lA,uA),
                                                        iB <- range (lB,uB),
                                                        iC <- range (lC,uC),
                                                        iD <- range (lD,uD),
                                                        iE <- range (lE,uE),
                                                        iF <- range (lF,uF)]


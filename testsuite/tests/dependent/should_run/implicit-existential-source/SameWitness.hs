{-# LANGUAGE DataKinds, GADTs #-}
module SameWitness where

import Filter
import Vector

sameWitness :: Vec n a -> Vec n a -> Bool
sameWitness _ _ = True

bad :: Hidden a -> Hidden a -> Bool
bad p q = sameWitness p q

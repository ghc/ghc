{-# LANGUAGE DataKinds, GADTs #-}
module AuditMain where

import Filter
import Identity
import LetFilter (letFilter, unbundle)
import Opening
import Vector
import qualified WitnessSchema as W

ignored :: Vec n Int -> Bool
ignored _ = True

observeUnused :: Hidden Int -> Bool
observeUnused p = ignored (forward p)

main :: IO ()
main = do
    let p = filterVec odd Nil :: Hidden Int
        q = filterVec odd (Cons 7 (Cons 9 Nil))
        lazy = filterVec odd (Cons 7 (error "inferred opening forced the tail"))
    print (audit p q, take 1 (toList (forward lazy)),
           observeUnused (error "identity forced the package"))
    print (take 1 (toList (letFilter odd (Cons (7 :: Int) (error "let forced the tail")))))
    print (unbundle (p, q), take 1 (fst (unbundle (lazy, error "unused second package"))))
    print (W.observeRankedInput viaFixedInput, W.observeRankedInput viaForwardedInput)

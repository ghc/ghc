{-# LANGUAGE DataKinds, GADTs #-}
module AuditMain where

import Filter
import Identity
import Opening
import Vector

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

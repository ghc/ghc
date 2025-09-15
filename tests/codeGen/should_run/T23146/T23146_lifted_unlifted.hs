{-# LANGUAGE GADTs, DataKinds #-}

import T23146_lifted_unliftedA

import Data.Type.Equality

fieldsSam :: NP True -> NP True -> Bool
fieldsSam (x' ::* xs) (y' ::* ys) = fieldsSam xs ys
fieldsSam (UNil Refl) (UNil Refl) = True

main :: IO ()
main = print (fieldsSam (UNil Refl) (UNil Refl))



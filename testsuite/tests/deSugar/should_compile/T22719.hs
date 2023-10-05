{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wall #-}

module T22719 where

import GHC.Exts

type T :: UnliftedType
data T = T

f :: Int -> T
f 0 = T
f n = f (n-1)

-- ex1 is lazy in (f 7)
ex1 :: ()
ex1 = let _ = f 7 in ()

-- ex2 is strict in (f 10)
ex2 :: ()
ex2 = let _a = f 10 in ()

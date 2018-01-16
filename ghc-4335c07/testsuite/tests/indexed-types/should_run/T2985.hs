{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wnot #-}

module Main where

-- See http://article.gmane.org/gmane.comp.lang.haskell.general/16796
-- and Trac #2985

instance (Num a, Num b, a ~ b) => Num (a,b) where
    (x,y) * (u,v) = (x*u-y*v, x*v+y*u)

test1 = (1,1) * (2,2)
main = print test1

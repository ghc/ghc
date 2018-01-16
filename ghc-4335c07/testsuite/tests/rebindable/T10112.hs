{-# LANGUAGE RankNTypes, RebindableSyntax #-}
module T10112 where

import qualified Prelude as P

(>>=) :: a -> ((forall b . b) -> c) -> c
a >>= f = f P.undefined
return a = a
fail s = P.undefined

t1 = 'd' >>= (\_ -> 'k')

t2 = do { _ <- 'd'
        ; 'k' }

foo = P.putStrLn [t1, t2]

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module SafeLang18 where

#define p377 toPair

data StrictPair a b = !a :*: !b

toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE p377 #-}

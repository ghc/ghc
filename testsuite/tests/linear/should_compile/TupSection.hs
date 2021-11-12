{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TupleSections #-}
module TupSection where
{-
inplace/bin/ghc-stage0 -O2 -dcore-lint
-}

myAp :: (a -> b) -> a -> b
myAp f x = f x

foo = myAp (,()) ()

qux = ("go2",) $ ()

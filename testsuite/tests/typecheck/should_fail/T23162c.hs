{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module T23162c where

type family Bak a = r | r -> a where
     Bak Int  = Char
     Bak Char = Int
     Bak a    = a

eq :: a -> a -> ()
eq x y = ()

bar :: (c->()) -> ()
bar =  bar

foo :: a -> Bak a
foo = foo

-- Bak alpha ~ ()
f :: ()
f = bar (\y -> eq (foo y) ())

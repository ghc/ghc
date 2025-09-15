{-# LANGUAGE TypeOperators, MultiParamTypeClasses, ExistentialQuantification #-}

module T15675 where

class a + b

data D1 = forall a b. (a + b) => D1 a b
data D2 = forall a b.  a + b  => D2 a b

class a ! b

data D3 = forall a b. (a ! b) => D3 !a !b
data D4 = forall a b.  a ! b  => D4 !a !b

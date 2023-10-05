{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module T12844 where

barWraper :: ('(r,r') ~ Head rngs, Foo rngs) => FooData rngs
barWraper = bar

bar :: (_) => FooData rngs
bar = foo

data FooData rngs

class Foo xs where foo :: (Head xs ~ '(r,r')) => FooData xs

type family Head (xs :: [k]) where Head (x ': xs) = x

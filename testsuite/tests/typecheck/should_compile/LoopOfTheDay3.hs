{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             OverlappingInstances, UndecidableInstances #-}

-- Instances compile fine but instance selection loops in GHC 6.2.
-- try: :t foo (T1a 1)
-- This is essentially the same as LoopOfTheDay2.hs
-- but with the innocent (?) use of overlapping instances.

module ShouldCompile where 

data T1 = T1a Int | T1b T1

class C0 x where foo :: x -> (); foo = undefined
class C1 x y
class C1 x y => C2 x y

instance C0 a                => C1 () a
instance (C1 x T1, C1 x Int) => C2 x T1
instance C1 x Int            => C2 x Int
instance C2 () a             => C0 a

baz = foo (T1b (T1a 3))

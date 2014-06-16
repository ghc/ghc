{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

-- Compiles fine.
-- Instance selection works fine.
-- try: :t foo (T1b T1a)

module ShouldCompile where

-- Notice: T1 is a recursive type.
-- Notice: the classes are recursive, too.
-- Why does this work when almost the same thing doesn't?
-- Say: adding an Int component to T1a makes things loop.
-- See LoopOfTheDay2.hs and LoopOfTheDay3.hs.

data T1 = T1a | T1b T1

class C0 x where foo :: x -> (); foo = undefined
class C1 x y
class C1 x y => C2 x y

instance C0 T1              => C1 () T1		-- (I1)
instance (C1 x T1)          => C2 x T1		-- (I2)
instance C2 () T1           => C0 T1		-- (I3)

baz = foo (T1b T1a)

{- Need 	C0 T1
-->(I3)		C2 () T1
-->(I2)		C1 () T1
-->(I1)		C0 T1		-- STOP because we've seen this before
-}

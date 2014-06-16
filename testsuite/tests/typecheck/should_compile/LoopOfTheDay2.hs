{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

-- Compilation loops in GHC 6.2!
-- While LoopOfTheDay1.hs did compile and work,
-- this one loops during compilation, even though
-- there is only an innocent difference regarding T1,
-- i.e., an additional, non-recursive constructor component.

module ShouldCompile where

data T1 = T1a Int | T1b T1

class C0 x where foo :: x -> (); foo = undefined
-- foo :: C0 x => x -> ()

class C1 x y
class C1 x y => C2 x y
		
instance C0 Int              => C1 () Int	-- I1
instance C0 T1               => C1 () T1	-- I2
instance (C1 x T1, C1 x Int) => C2 x T1		-- I3
instance C1 x Int            => C2 x Int	-- I4
instance C2 () T1            => C0 T1		-- I5
instance C2 () Int           => C0 Int		-- I6


baz = foo (T1b (T1a 3))

{- Need
		C0 T1
-->(I5)		C2 () T1
-->(I3)		C1 () T1, C1 () Int
-->(I1,I2)	C0 T1, C0 Int
-->(recusive)	C0 Int
-->(I6)		C2 () Int
-->(I4)		C1 () Int
-->(recursive)	{}
-}		

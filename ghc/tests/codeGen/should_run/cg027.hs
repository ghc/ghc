-- !!! simple test of 0-method classes
--

class (Num a, Integral a) => Foo a

main = putStr (shows (f ((fromInteger 21)::Int)
			((fromInteger 37))) "\n")

instance Foo Int

f :: Foo a => a -> a -> Integer

f a b = toInteger (a + b)

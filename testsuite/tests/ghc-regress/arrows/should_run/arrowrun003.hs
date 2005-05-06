{-# OPTIONS -farrows #-}

module Main(main) where

import Control.Arrow

class ArrowLoop a => ArrowCircuit a where
	delay :: b -> a b b

-- stream map instance

data Stream a = Cons a (Stream a)

instance Functor Stream where
	fmap f ~(Cons a as) = Cons (f a) (fmap f as)

zipStream :: Stream a -> Stream b -> Stream (a,b)
zipStream ~(Cons a as) ~(Cons b bs) = Cons (a,b) (zipStream as bs)

unzipStream :: Stream (a,b) -> (Stream a, Stream b)
unzipStream abs = (fmap fst abs, fmap snd abs)

newtype StreamMap a b = StreamMap (Stream a -> Stream b)
unStreamMap (StreamMap f) = f

instance Arrow StreamMap where
	arr f = StreamMap (fmap f)
	StreamMap f >>> StreamMap g = StreamMap (g . f)
	first (StreamMap f) =
		StreamMap (uncurry zipStream . first f . unzipStream)

instance ArrowLoop StreamMap where
	loop (StreamMap f) =
		StreamMap (loop (unzipStream . f . uncurry zipStream))

instance ArrowCircuit StreamMap where
	delay a = StreamMap (Cons a)

listToStream :: [a] -> Stream a
listToStream = foldr Cons undefined

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a:streamToList as

runStreamMap :: StreamMap a b -> [a] -> [b]
runStreamMap (StreamMap f) as =
	take (length as) (streamToList (f (listToStream as)))

-- simple automaton instance

data Auto a b = Auto (a -> (b, Auto a b))

instance Arrow Auto where
	arr f = Auto $ \a -> (f a, arr f)
	Auto f >>> Auto g = Auto $ \b ->
				let	(c, f') = f b
					(d, g') = g c
				in	(d, f' >>> g')
	first (Auto f) = Auto $ \(b,d) -> let (c,f') = f b in ((c,d), first f')

instance ArrowLoop Auto where
	loop (Auto f) = Auto $ \b ->
		let	(~(c,d), f') = f (b,d)
		in	(c, loop f')

instance ArrowCircuit Auto where
	delay a = Auto $ \a' -> (a, delay a')

runAuto :: Auto a b -> [a] -> [b]
runAuto (Auto f) [] = []
runAuto (Auto f) (a:as) = let (b, f') = f a in b:runAuto f' as

-- Some simple example circuits

-- A resettable counter (first example in several Hawk papers):

counter :: ArrowCircuit a => a Bool Int
counter = proc reset -> do
	rec	output <- returnA -< if reset then 0 else next
		next <- delay 0 -< output+1
	returnA -< output

-- Some other basic circuits from the Hawk library.

-- flush: when reset is True, return d for n ticks, otherwise copy value.
-- (a variation on the resettable counter)

flush :: ArrowCircuit a => Int -> b -> a (b, Bool) b
flush n d = proc (value, reset) -> do
	rec	count <- returnA -< if reset then n else max (next-1) 0
		next <- delay 0 -< count
	returnA -< if count > 0 then d else value

-- latch: on each tick, return the last value for which reset was True,
-- or init if there was none.
--  
latch :: ArrowCircuit a => b -> a (b, Bool) b
latch init = proc (value, reset) -> do
	rec	out <- returnA -< if reset then value else last
		last <- delay init -< out
	returnA -< out

-- Some tests using the counter

test_input = [True, False, True, False, False, True, False, True]
test_input2 = zip [1..] test_input

-- A test of the resettable counter.

main = do
	print (runStreamMap counter test_input)
	print (runAuto counter test_input)
	print (runStreamMap (flush 2 0) test_input2)
	print (runAuto (flush 2 0) test_input2)
	print (runStreamMap (latch 0) test_input2)
	print (runAuto (latch 0) test_input2)

-- A step function (cf current in Lustre)

step :: ArrowCircuit a => b -> a (Either b c) b
step b = proc x -> do
		rec	last_b <- delay b -< getLeft last_b x
		returnA -< last_b
	where	getLeft _ (Left b) = b
		getLeft b (Right _) = b

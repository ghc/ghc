{-  The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Louis Wasserman
    
    This should be compiled with:
    	-threaded -O2 -fexcess-precision -fasm
    and run with:
    	+RTS -N<number of cores> -RTS <input>
-}

import Control.Concurrent
import Control.Monad
import System.Environment
import Foreign hiding (rotate)
import Data.Semigroup

type Perm = Ptr Word8

data F = F {-# UNPACK #-} !Int {-# UNPACK #-} !Int

instance Semigroup F where
	F s1 m1 <> F s2 m2 = F (s1 + s2) (max m1 m2)

instance Monoid F where
	mempty = F 0 0

incPtr = (`advancePtr` 1)
decPtr = (`advancePtr` (-1))

flop :: Int -> Perm -> IO ()
flop k xs = flopp xs (xs `advancePtr` k)
 where flopp i j = when (i < j) $ swap i j >> flopp (incPtr i) (decPtr j)
       swap i j = do
	a <- peek i
	b <- peek j
	poke j a
	poke i b

flopS :: Perm -> (Int -> IO a) -> IO a
flopS !xs f = do
	let go !acc = do
		k <- peekElemOff xs 0
		if k == 0 then f acc else flop (fromIntegral k) xs >> go (acc+1)
	go 0

increment :: Ptr Word8 -> Ptr Word8 -> IO ()
increment !p !ct = do
	first <- peekElemOff p 1
	pokeElemOff p 1 =<< peekElemOff p 0
	pokeElemOff p 0 first
	
	let go !i !first = do
		ci <- peekElemOff ct i
		if fromIntegral ci < i then pokeElemOff ct i (ci+1) else do
			pokeElemOff ct i 0
			let !i' = i + 1
			moveArray p (incPtr p) i'
			pokeElemOff p i' first
			go i' =<< peekElemOff p 0
	go 1 first  

genPermutations :: Int -> Int -> Int -> Ptr Word8 -> Ptr Word8 -> IO F
genPermutations !n !l !r !perm !count = allocaArray n $ \ destF -> do
	let upd j !f run = do
		p0 <- peekElemOff perm 0
		if p0 == 0 then increment perm count >> run f else do
			copyArray destF perm n
			increment perm count
			flopS destF $ \ flops -> 
				run (f `mappend` F (checksum j flops) flops)
	let go j !f = if j >= r then return f else upd j f (go (j+1))
	go l mempty
 where checksum i f = if i .&. 1 == 0 then f else -f

facts :: [Int]
facts = scanl (*) 1 [1..12]

unrank :: Int -> Int -> (Ptr Word8 -> Ptr Word8 -> IO a) -> IO a
unrank !idx !n f = allocaArray n $ \ p -> allocaArray n $ \ count ->
  allocaArray n $ \ pp -> do
	mapM_ (\ i -> pokeElemOff p i (fromIntegral i)) [0..n-1]
	let go i !idx = when (i >= 0) $ do
		let fi = facts !! i
		let (q, r) = idx `quotRem` fi
		pokeElemOff count i (fromIntegral q)
		copyArray pp p (i+1)
		let go' j = when (j <= i) $ do
			let jq = j + q
			pokeElemOff p j =<< peekElemOff pp (if jq <= i then jq else jq - i - 1)
			go' (j+1)
		go' 0
		go (i-1) r
	go (n-1) idx
	f p count

main = do
   n <- fmap (read.head) getArgs
   let fact = product [1..n]
   let bk = fact `quot` 4
   vars <- forM [0,bk..fact-1] $ \ ix -> do
   	var <- newEmptyMVar
   	forkIO (unrank ix n $ \ p -> genPermutations n ix (min fact (ix + bk)) p >=> putMVar var)
   	return var
   F chksm mflops <- liftM mconcat (mapM takeMVar vars)
   putStrLn $ (show chksm) ++ "\nPfannkuchen(" ++ (show n) ++ ") = " ++ (show $ mflops)

{-# OPTIONS -XRecursiveDo #-}

-- test that we have all the promised instances

module Main(main) where
 
import Control.Monad.Fix 
import qualified Control.Monad.ST      as SST
import qualified Control.Monad.ST.Lazy as LST

generic :: MonadFix m => m [Int]
generic = mdo xs <- return (1:xs)
	      return (take 4 xs)

io :: IO [Int]
io = generic

sst :: SST.ST s [Int]
sst = generic

lst :: LST.ST s [Int]
lst = generic
	
mb :: Maybe [Int]
mb = generic

ls :: [[Int]]
ls = generic

main :: IO ()
main = do 
	print	=<< io
	print	$   SST.runST sst	
	print	$   LST.runST lst
	print	$   mb	
	print	$   ls

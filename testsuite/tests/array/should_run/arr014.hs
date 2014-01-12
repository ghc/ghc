-- !!! multi-dimensional arrays

module Main ( main ) where
import Control.Monad.ST
import Data.Array.ST

type TwoD s = STArray s Int (STArray s Int Int)

setup :: ST s (TwoD s)
setup = let isz = 10
            imax = isz - 1	  
            osz = 2
            omax = osz - 1 in
	do
            -- gives : undefined reference to `IOBase_error_closure'
--	    x <- newArray (0, omax) (error "uninitialised")
	    dmy <- newArray (0, imax) 0      
	    x <- newArray (0, omax) dmy
	    as <- (sequence . replicate osz) (newArray (0, imax) 6)
	    mapM_ (\(i,v) -> writeArray x i v) (zip [0..omax] as)
	    return x	  

main :: IO ()
main = do
         a <- stToIO setup
	 return ()	 

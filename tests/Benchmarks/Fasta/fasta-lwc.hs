{-  The Computer Language Benchmarks Game

    http://benchmarkgame.alioth.debian.org/

    contributed by Bryan O'Sullivan
-}

import Control.Monad
import Data.ByteString.Unsafe
import Foreign.Ptr
import Foreign.Storable
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

main = do
    n <- getArgs >>= readIO.head
    writeAlu ">ONE Homo sapiens alu" (L.take (fromIntegral n*2) (L.cycle alu))
    make ">TWO IUB ambiguity codes" (n*3) iub 42 >>=
      void . make ">THREE Homo sapiens frequency" (n*5) homosapiens

writeAlu name s0 = B.putStrLn name >> go s0
 where go s = L.putStrLn h >> unless (L.null t) (go t)
         where (h,t) = L.splitAt 60 s

make name n0 tbl seed0 = do
  B.putStrLn name
  let modulus = 139968
      fill ((c,p):cps) j =
	let !k = min modulus (floor (fromIntegral modulus * (p::Float) + 1))
	in B.replicate (k - j) c : fill cps k
      fill _ _ = []
      lookupTable = B.concat $ fill (scanl1 (\(_,p) (c,q) -> (c,p+q)) tbl) 0
      line = B.replicate 60 '\0'
  unsafeUseAsCString line $ \ptr -> do
    let make' n !i seed
	    | n > (0::Int) = do
		let newseed = rem (seed * 3877 + 29573) modulus
		plusPtr ptr i `poke` unsafeIndex lookupTable newseed
		if i+1 >= 60
		    then puts line 60 >> make' (n-1) 0 newseed
		    else make' (n-1) (i+1) newseed
	    | otherwise = when (i > 0) (puts line i) >> return seed
    make' n0 0 seed0

alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
      ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
      ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens = [('a',0.3029549426680),('c',0.1979883004921)
              ,('g',0.1975473066391),('t',0.3015094502008)]

puts bs n = B.putStrLn (B.take n bs)


Time-stamp: <Wed May 22 1996 19:30:00 Stardate: [-31]7543.85 hwloidl>

Computing a Fast Fourier Transformation.

Based on the NESL code presented in:
   Programming Parallel Algorithms
   by Guy E. Blelloch 
   in CACM 39(3), March 1996
   URL: http://www.cs.cmu.edu/afs/cs.cmu.edu/project/scandal/public/www/nesl/alg-numerical.html

The following description is taken from the original NESL code:

  This is a simple parallel version of the standard sequential fast Fourier
  transform algorithm. The algorithm does O(n log n) work and has O(log n)
  depth.
  
  In the code we first give a general FFT that works with any commutative
  ring. As well as the data, this FFT takes as arguments the n roots of unity
  (an array w) and the +, * functions on the ring (add, mult). We then
  specialize the algorithm to a particular commutative ring---the complex
  numbers. Multiline text area Submit

NB: THIS IS UNTESTED CODE %--  HWL

\begin{code}
#if defined(GRAN)
import Strategies
#endif
#if defined(ARGS)
import GranRandom
import LibSystem       -- for getArgs
#endif

even_elts []     = []
even_elts [x]    = [x]
even_elts xs@(x:_) = x : (even_elts (drop 2 xs))

odd_elts [] = []
odd_elts (x:xs) = even_elts xs
\end{code}

The main function, parameterised with function for adding and multiplying 
two elements (with Haskell's class system that shouldn't be necessary).

\begin{code}
-- fft :: Integral a => [a] -> [a] -> [a]
{- SPECIALISE fft :: [Complex Double] -> [Complex Double] -> [Complex Double] -}
fft :: [Complex Double] -> [Complex Double] -> [Complex Double]
fft a w  
  | length a <= 1  =  a
  | otherwise      = let r0 = {-# SCC "head" #-} fft (even_elts a) (even_elts w)
			 r1 = {-# SCC "head" #-} fft (odd_elts a) (even_elts w)
                         z =  {-# SCC "zip3" #-}  zip3 (r0++r0) (r1++r1) w
		     in 
#if defined(GRAN)
                        parList rnf r0 `par`    
                        parList rnf r1 `par`
			parList rwhnf z  `par`
                        parList rnf     
                        --_parGlobal_ 11# 11# 0# 0#  (rnf r0) $
                        --_parGlobal_ 12# 12# 0# 0#  (rnf r1) $
                        --_parGlobal_ 13# 13# 0# 0#  (rnf z) $
#endif
		        [ r0'+r1'*w' 
		    	| (r0',r1',w') <- z ]

complex_fft :: [Complex Double] -> [Complex Double]
complex_fft a =
  let 
      c :: Double  
      c = (2.0*pi)/(fromIntegral (length a))
      w = {-# SCC "w" #-}  [ (cos (c*(fromIntegral i)) :+ sin (c*(fromIntegral i)) ) 
                           | i <- [0..length a] ]
      -- add = \ (ar,ai) (br,bi) -> (ar+br,ai+bi)
      -- mult = \ (ar,ai) (br,bi) -> (ar*br-ai*bi,ar*bi+ai*br)
  in (rnf w) `seq` fft a w 
\end{code}

Test data.

\begin{code}
x0 :: [Complex Double]
x0 = [(2.0 :+ 0.0),(-1.0 :+ 1.0),(0.0 :+ 0.0),(-1.0 :+ -1.0)]

-- 64 elems with values between 0 and 99
x1 :: [Complex Double]
x1 = [ (67.0 :+ -17.0), (80.0 :+ 53.0), (-58.0 :+ 45.0), (16.0 :+ 96.0), (-15.0 :+ -4.0), (83.0 :+ 91.0), (-31.0 :+ -44.0), (96.0 :+ -37.0), (-63.0 :+ -99.0), (70.0 :+ 1.0), (-12.0 :+ 38.0), (-57.0 :+ 5.0), (-84.0 :+ -94.0), (31.0 :+ -47.0), (70.0 :+ -11.0), (41.0 :+ 24.0), (-99.0 :+ -74.0), (-59.0 :+ -41.0), (58.0 :+ -83.0), (27.0 :+ 25.0), (-38.0 :+ -32.0), (78.0 :+ -18.0), (-95.0 :+ 37.0), (68.0 :+ -42.0), (-61.0 :+ 92.0), (-34.0 :+ 16.0), (-66.0 :+ -52.0), (39.0 :+ 20.0), (-1.0 :+ -45.0), (69.0 :+ 47.0), (39.0 :+ -78.0), (-78.0 :+ -12.0), (81.0 :+ -96.0), (-88.0 :+ 20.0), (31.0 :+ 20.0), (-22.0 :+ -46.0), (-54.0 :+ 59.0), (53.0 :+ 16.0), (-96.0 :+ 55.0), (44.0 :+ -77.0), (48.0 :+ -49.0), (83.0 :+ -6.0), (77.0 :+ 49.0), (20.0 :+ 46.0), (21.0 :+ 9.0), (-40.0 :+ 78.0), (-57.0 :+ -65.0), (-12.0 :+ 21.0), (7.0 :+ -74.0), (36.0 :+ 87.0), (-86.0 :+ 0.0), (-6.0 :+ 21.0), (-15.0 :+ -2.0), (45.0 :+ -26.0), (18.0 :+ -65.0), (12.0 :+ -39.0), (-36.0 :+ -68.0), (49.0 :+ 6.0), (95.0 :+ -92.0), (74.0 :+ -44.0), (-30.0 :+ -95.0), (-26.0 :+ 86.0), (84.0 :+ -61.0), (-64.0 :+ -28.0) ]

\end{code}

Main function.

\begin{code}
args_to_IntList a = if length a < 2
		      then error "Usage: fft <lst-length> <max-elem>\n"
		      else map (\ a1 -> fst ((readDec a1) !! 0)) a

#if defined(ARGS)
munch_args = 	getArgs >>= \a ->
                return (args_to_IntList a) >>= \[n,m] ->
                getRandomDoubles (fromIntegral m) >>= \ random_list -> 
	        let 
                  (l1, random_list') = splitAt n random_list
                  (l2, random_list'') = splitAt n random_list'
                  x = zipWith (\ x y -> (x :+ y)) l1 l2
		in
		return (x)

#else
munch_args = return (x1)
#endif

#ifdef PRINT
main = munch_args >>= \ x -> print (complex_fft x)
#else
main = munch_args >>= \ x -> (rnf $ complex_fft x) `seq` putStr "Done\n"
#endif

\end{code}

-----------------------------------------------------------------------------

This is the original NESL code:

function fft(a,w,add,mult) =
if #a == 1 then a
else
  let r = {fft(b, even_elts(w), add, mult):
           b in [even_elts(a),odd_elts(a)]}
  in {add(a, mult(b, w)):
      a in r[0] ++ r[0]; 
      b in r[1] ++ r[1];
      w in w};

function complex_fft(a) =
let 
    c = 2.*pi/float(#a);
    w = {cos(c*float(i)),sin(c*float(i)) : i in [0:#a]};
    add = ((ar,ai),(br,bi)) => (ar+br,ai+bi);
    mult = ((ar,ai),(br,bi)) => (ar*br-ai*bi,ar*bi+ai*br);
in fft(a,w,add,mult);

complex_fft([(2.,0.),(-1.,1.),(0.,0.),(-1.,-1.)]);

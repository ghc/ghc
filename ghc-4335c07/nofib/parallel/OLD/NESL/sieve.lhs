Time-stamp: <Sat May 18 1996 00:40:54 Stardate: [-31]7519.93 hwloidl>

Computing a list of prime numbers using the Sieve of Erathostenes.

Based on the NESL code presented in:
   Programming Parallel Algorithms
   by Guy E. Blelloch 
   in CACM 39(3), March 1996
   URL: http://www.cs.cmu.edu/~scandal/nesl/alg-numerical.html

\begin{code}
#if defined(GRAN)
import Strategies
#endif
\end{code}

\begin{code}
primes :: Integer -> [Integer]
primes 2 = []
primes n = 
  let sqr_primes = primes (ceiling (sqrt (fromInteger n)))
      sieves = [ [2*p, 3*p .. n]  | p <- sqr_primes ] 
      flat_sieves = concat sieves
      result  = [ i | i <- [0..n], i `notElem` flat_sieves ]
  in 
#if defined(GRAN)
  parList rnf sqr_primes `par`
  parList rnf flat_sieves `par`
  parList rnf result     `par`
#endif
  drop 2 result

main = print (primes 1000)
\end{code}

----------------------------------------------------------------------

The original NESL code:

function primes(n) =
if n == 2 then [] int
else 
  let sqr_primes = primes(ceil(sqrt(float(n))));
      sieves = {[2*p:n:p]: p in sqr_primes};
      flat_sieves = flatten(sieves);
      flags  = dist(t,n) <- {(i,f): i in flat_sieves};
  in drop({i in [0:n]; flags | flags}, 2) ;

primes(100);

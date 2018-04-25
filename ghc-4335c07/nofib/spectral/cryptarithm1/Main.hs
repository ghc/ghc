{- From Sergey Mechveliani, Oct 99

This pretends to be the "fair" improved Cryptarithm solver  test for
the performance comparison between  Haskell  and  C++.

--------------------------------------------------------------------
Compilation:   g++       -O3                       t.cc
               ghc-4.04  -c -fvia-C -O2 -O2-for-C  t.hs

RESULTS:    Platform1 -   C++  is  15 times faster,
            Platform2 -            10 times faster,

   Platform1:   PC i-586,  Linux Debian
           g++  version:
           g++ -v  says
            `gcc version egcs-2.90.29 980515 (egcs-1.0.3 release)'

   Platform2:  some machine with larger Cache.


I thank  Fergus Henderson <fjh@cs.mu.oz.au>

for the improvements in the C++ program and for suggesting to use the
list comprehensions in `permutations' (this saved another 10-15% of
cost).

The test shows the performance ratio  
                       CC++ / Haskell (ghc-4.04)   between 10 and 15

- it varies depending on the platform and other features.

It would be interesting to observe your running results, remarks,
comparison to other systems.

What is the meaning of such test? 
Comparing what is better an orange or an apple?

To my mind, this reflects the performance cost of the benefits of 
a higher level, functional language.
And it is chosen an unlucky task example for Haskell.
The nature of this task is so that it allows to generate 
permutations "in place", by updating the C++ vector.
I expect the smaller ratio for other, "average" tasks.

And it is interesting, how the functional compiler of future might 
optimize the below program. How essentially it could reduce the 
cost ratio?

--------------------------------------------------------------------
The  Cryptarithm solver test was proposed to the Haskell e-mail list 

by  Mark Engelberg <mark.engelberg@bigfoot.com>  
on  17 September 1999.

This is actually the test for the speed of the permutation 
generator program.
Mark Engelberg spoke of the task of finding first permutation
satisfying certain equation.
And he compared the Haskell program with the C++ program that uses
the  next_permutation  library function.

This comparison was incorrect, because it was not known whether the
Haskell and C++ programs test the same number of permutations before
finding the solution. For, it was not known in what order 
next_permutation  generates the permutations.
  ------------------------------------------------------------------
  Below follow the programs for the improved test:

  find  ALL  the permutations on  [0..9]  satisfying the condition
  \[t,h,i,r,y,w,e,l,v,n] ->
                      expand t h i r t y + 5 * expand t w e l v e ==
                      expand n i n e t y
      where
      expand a b c d e f = f +e*10 +d*100 +c*1000 +b*10000 +a*100000
  ------------------------------------------------------------------
The real difference makes only this "ALL" part:
all the permutations are tested - though only one satisfies the 
condition.
The differences to the original programs are as follows.

* Both programs test each of 10! permutations.
* The below Haskell program seems to generate the permutations 2-3 
  times faster than the original program.
* The C++ program uses the loop 
                              do {...} while (next_permutation(...))
  to list the solutions (it terminates when all the permutations
  are listed).

One amazing point: consider the last equation of `permutations':

                           ...= (j:k:ks): [(k:aks) | aks <- addj ks]

Replacing it with          ...  ...     : (map (k:) $ addj ks)
slows it down in 20% in ghc-4.04.

Fergus Henderson also tried Mercury, which showed somewhat higher
performance, especially, whith "memory recover by backtracking".

Fergus, could you show the test results? 
I mean the final source program in Mercury, timings, platform,
versions.

------------------
Sergey Mechveliani
mechvel@botik.ru

-}


-- Haskell ---------------------------------------------------------

main = putStr $ shows (filter condition $ permutations p0) "\n"
         where
         p0                              = [0..9] :: [Int]
         condition [t,h,i,r,y,w,e,l,v,n] =
                      expand t h i r t y + 5 * expand t w e l v e ==
                      expand n i n e t y

expand a b c d e f = f + e*10 + d*100 + c*1000 + b*10000 + a*100000
                     :: Int

permutations :: [Int] -> [[Int]]
            -- build the full permutation list given an ordered list

permutations []     = [[]]
permutations (j:js) = [r | pjs <- permutations js, r <- addj pjs]
                  where                   
                  addj []     = [[j]]
                  addj (k:ks) = (j:k:ks): [(k:aks) | aks <- addj ks]

{-
-- C++  ------------------------------------------------------------

#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

inline long expand (long a, long b, long c, long d, long e, long f)
{
 return f+10*e+100*d+1000*c+10000*b+100000*a;
}

int main()
{
 long  t,h,i,r,y,w,e,l,v,n;

 long temp[10] = {0,1,2,3,4,5,6,7,8,9};
 vector<long> x(temp,temp+10);
 do
   {t = x[0];  h = x[1];  i = x[2];  r = x[3];  y = x[4];
    w = x[5];  e = x[6];  l = x[7];  v = x[8];  n = x[9];

    if (expand(n,i,n,e,t,y) ==
                         expand(t,h,i,r,t,y) + 5*expand(t,w,e,l,v,e)
       )
     cout << t << h << i << r << y << w << e << l << v << n << '\n';
   }
   while ( next_permutation(x.begin(), x.end()) );
 cout << "FINISHED\n";
}

-}

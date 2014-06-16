-- !!! a performance-problem test from Jules.
--  further comment at the end
-- 
module Main where 

import Data.Char -- 1.3

--1.3:data Maybe a = Nothing | Just a

data ATree a b = ALeaf
               | ABranch (ATree a b) a [b] (ATree a b) Int
                 -- deriving (Eq)

type SymTable = ATree String Int


pp_tree :: SymTable -> String
pp_tree ALeaf = ""
pp_tree (ABranch l k vs r h)
  = pp_tree l ++ show (k,reverse vs) ++ "\n" ++ pp_tree r

{-
avAdd :: Ord a  =>  ATree a b -> 
                    a -> 
                    b -> 
                    ATree a b
-}
avAdd ALeaf xk xv = ABranch ALeaf xk [xv] ALeaf 1

avAdd (ABranch l yk yv r hy) xk xv
   | yk > xk = let (ABranch l1 zk zv l2 _) = avAdd l xk xv
               in avCombine l1 (f l1) l2 (f l2) r (f r) zk zv yk yv
   | xk > yk = let (ABranch r1 zk zv r2 _) = avAdd r xk xv
               in avCombine l (f l) r1 (f r1) r2 (f r2) yk yv zk zv
   | otherwise  = ABranch l yk (xv:yv) r hy
   where
      f :: ATree a b -> Int
      f ALeaf = 0
      f (ABranch _ _ _ _ d) = d
      


-- ==========================================================--
--
{-
avLookup :: Ord a  =>  ATree a b -> 
                       a -> 
                       Maybe b
-}
avLookup ALeaf _ = Nothing

avLookup (ABranch l k v r _) kk
   | kk < k     = avLookup l kk
   | kk > k     = avLookup r kk
   | otherwise  = Just v



-- ==========================================================--
--
avCombine :: ATree a b -> 
             Int -> 
             ATree a b -> 
             Int -> 
             ATree a b -> 
             Int -> 
             a -> 
             [b] -> 
             a -> 
             [b] -> 
             ATree a b

avCombine t1 h1 t2 h2 t3 h3 ak av ck cv
   | h2 > h1 && h2 > h3
      = ABranch (ABranch t1 ak av t21 (h1+1)) bk bv 
                (ABranch t22 ck cv t3 (h3+1)) (h1+2)
   | h1 >= h2 && h1 >= h3
      = ABranch t1 ak av (ABranch t2 ck cv t3 (max1 h2 h3)) 
                (max1 h1 (max1 h2 h3))
   | h3 >= h2 && h3 >= h1
      = ABranch (ABranch t1 ak av t2 (max1 h1 h2)) ck cv t3 
                (max1 (max1 h1 h2) h3)
   where
      (ABranch t21 bk bv t22 _) = t2
      max1 :: Int -> Int -> Int
      max1 n m = 1 + (if n > m then n else m)


-- ==========================================================--
-- === end                                     AVLTree.hs ===--
-- ==========================================================--




xref :: SymTable -> Int -> String -> SymTable

xref stab lineno [] = stab
xref stab lineno ('\n':cs) = xref stab (lineno+1) cs
xref stab lineno (c:cs) 
   = if isAlpha c then 
        let (word, rest) = span isAlphaNum cs
        in  xref (avAdd stab (c:word) lineno) lineno rest
     else xref stab lineno cs

main = do
    s <- getContents
    putStr (pp_tree (xref ALeaf 1 s))

{-
Date: Thu, 29 Oct 92 19:38:31 GMT
From: Julian Seward (DRL PhD) <sewardj@uk.ac.man.cs>
Message-Id: <9210291938.AA27685@r6b.cs.man.ac.uk>
To: partain@uk.ac.glasgow.dcs
Subject: More ghc vs hbc fiddling (OR: nofib ephemeral contribution (unsolicited :-))

Will,

There are still some very simple programs for which ghc's performance
falls far behind that of hbc's -- even with ghc using a better
GC.  The stat files below are from a 
crude cross reference program we hacked together for the purposes
of an internal "what-language-to-teach-first-year-undergrads" debate.

Is this something to do with dictionary zapping?

Program included below.  Use as a pipe.  Suggest you feed it any
large Haskell source file (I used TypeCheck5.hs from Anna).

Jules

---------------------------------------------------------

a.out -H9000000 -S 
Nw Heap Tt Heap   Stk    GC(real) GC acc (real)     tot (real) newheap    in -dupl  -new  -del  +stk   out  mcode
  99192   99192    20  0.06   0.1   0.06    0.1    0.16    0.4  396768     0     0     0     0     0     0
 247752  247752    14  0.13   0.1   0.19    0.2    0.44    0.8  991008     0     0     0     0     0     0
 623104  623104    34  0.32   0.3   0.51    0.5    1.08    1.5 2492416     0     0     0     0     0     0
1433968 1433968 15879  0.62   0.8   1.13    1.4    2.66    3.6 5735872     0     0     0     0     0     0
3009700 3009700  2382  1.56   1.6   2.69    3.0    6.88    8.6 9000000     0     0     0     0     0     0
         5 GCs,
      8.69 (13.1) seconds total time,
      2.69 (3.0) seconds GC time (31.0(23.1)% of total time)
      0.00 (0.0) seconds major GC time ( 0.0( 0.0)% of total time)
   9303816 bytes allocated from the heap.

------------------------------------------------

xref +RTS -H9M -S -K200k 

Collector: APPEL  HeapSize: 9,437,184 (bytes)

  Alloc   Live   Live   Astk   Bstk OldGen   GC    GC     TOT     TOT  Page Flts  Collec  Resid
  bytes   bytes    %   bytes  bytes  roots  user  elap    user    elap   GC  TOT   tion   %heap
4718580  786672  16.7     40    220    424  0.37  0.52    3.67    4.68    0    0   Minor
4325248  808804  18.7  62724  62820 564968  0.50  0.60    6.63    8.05    0    0   Minor
3920848  743508  19.0  47512  47600 743220  0.47  0.60    8.60   10.17    0    0   Minor
3549096  681464  19.2  34644  34892 680820  0.46  0.53   10.43   12.13    0    0   Minor
3208348  604892  18.9  23564  23676 604512  0.41  0.48   12.07   13.89    0    0   Minor
2905900  528584  18.2  14164  14396 527952  0.35  0.41   13.53   15.42    0    0   Minor
2641592  490812  18.6   5228   5388 490476  0.30  0.37   14.85   16.82    0    0   Minor
2396204  534400  22.3     16     40 534380  0.28  0.32   16.41   18.75    0    0   Minor
2129016  691708  32.5     36    144 691420  0.33  0.39   18.38   21.68    0    0   Minor
1090480

30,885,312 bytes allocated in the heap
         9 garbage collections performed

  Total time  19.29s  (23.06s elapsed)
  GC time      3.47s  (4.22s elapsed)
  %GC time    18.0%

--------------------------------------------------
-}

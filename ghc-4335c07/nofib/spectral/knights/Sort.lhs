\section{Introduction}
This module defines, and tests various sort functions written is Haskell.
I descided to do this rather trivial exercise, to accustom myself with
the prototype Glasgow Haskell compiler, and Glasgows literate programming
system. 

Use of this module is obtained by importing the module, and {\em one} of the
sort functions which will be renamed to {\tt sort} - 
i.e ``{\tt import Sort(lazySort) renaming sort}'' imports {\tt lazySort},
which can be referenced in the current scope as {\tt sort}.

\begin{code}
module Sort(insertSort,mergeSort,quickSort,lazySort)  where

\end{code}
%%%%%%%%%%%%%%%%%%%%%%I n s e r t i o n   S o r t%%%%%%%%%%%%%%%%%%%%%%
\section{Insertion Sort}
The following piece of code is taken from the Gofer manual.
%
\begin{code}
insertSort::(Ord a) => [a] -> [a]
insertSort xs = foldr insertion [] xs

insertion :: (Ord a) => a -> [a] -> [a]
insertion x [] = [x]
insertion x (y:ys) 
	| x <= y    = x:y:ys
	| otherwise = y:insertion x ys
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%M e r g e   S o r t%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Merge Sort}
The following code is taken from Keiths ''Functional Programming 2''
lecture notes. 
%
\begin{code}
mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs
	= if (n <=1 ) then xs 
          else 
             (mergeList 
		( mergeSort (take (n `div` 2) xs))
		( mergeSort (drop (n `div` 2) xs)))
	  where
	    	n = length xs

mergeList :: (Ord a) => [a] -> [a] -> [a]
mergeList []   ys = ys
mergeList xs   [] = xs
mergeList (x:xs) (y:ys)
	| x <= y    = x:mergeList xs (y:ys)
	| otherwise = y:mergeList (x:xs) ys
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%Q u i c k   S o r t%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{A not so ``Quick Sort''}
Standard definition of quicksort\cite{turner} using list comprehensions.
\begin{code}
quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = (quickSort [y | y<-xs, y<x]) ++ [x] ++ 
                   (quickSort [y | y<-xs, y>=x])
\end{code}
%%%%%%%%%%%%%%%%%%%%%%L e n n a r t s  S o r t%%%%%%%%%%%%%%%%%%%%%%%
\section{Lennart Augustsson's ``Quick Sort''}
   This module implements a sort function using a variation on
   quicksort.  It is stable, uses no concatenation and compares
   only with {\tt <=}.
%
\begin{rawlatex}
\begin{itemize}
\item {\tt sortLe} sorts with a given predicate 
\item {\tt lazySort} uses the {\tt <=} method
\end{itemize}
\end{rawlatex}
%
\begin{code}
lazySortLe :: (a -> a -> Bool) -> [a] -> [a]
lazySortLe le l = lazyQsort le   l []

lazySort :: (Ord a) => [a] -> [a]
lazySort      l = lazyQsort (<=) l []

-- lazyQsort is stable and does not concatenate.
lazyQsort :: (a -> a -> Bool) -> [a] -> [a] -> [a]
lazyQsort le []     r = r
lazyQsort le [x]    r = x:r
lazyQsort le (x:xs) r = qpart le x xs [] [] r

-- rlazyQsort is as lazyQsort but anti-stable, 
-- i.e. reverses equal elements.
rlazyQsort :: (a -> a -> Bool) -> [a] -> [a] -> [a]
rlazyQsort  le []     r = r
rlazyQsort le [x]    r = x:r
rlazyQsort  le (x:xs) r = rqpart le x xs [] [] r

-- qpart partitions and sorts the sublists
-- rlt and rge are in reverse order and must be sorted with an
-- anti-stable sorting
qpart :: (a -> a -> Bool) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart le x [] rlt rge r =
    rlazyQsort le rlt (x:rlazyQsort le rge r)
qpart le x (y:ys) rlt rge r =
    if le x y then
	qpart le x ys rlt (y:rge) r
    else
	qpart le x ys (y:rlt) rge r

rqpart :: (a -> a -> Bool) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart le x [] rle rgt r =
    lazyQsort le rle (x:lazyQsort le rgt r)
rqpart le x (y:ys) rle rgt r =
    if le y x then
	rqpart le x ys (y:rle) rgt r
    else
	rqpart le x ys rle (y:rgt) r
\end{code}
%%%%%%%%%%%%%%%%%%%%%%R a n d o m  N u m b e r s%%%%%%%%%%%%%%%%%%%%%%%
\section{Random Number Generator}
   This module implements a (good) random number generator.

   The June 1988 (v31 \#6) issue of the Communications of the ACM has an
   article by Pierre L'Ecuyer called, "Efficient and Portable Combined
   Random Number Generators".  Here is the Portable Combined Generator of
   L'Ecuyer for 32-bit computers.  It has a period of roughly 2.30584e18.

   Transliterator: Lennart Augustsson

Use seeds s1 in 1..2147483562 and s2 in 1..2147483398 to generate
an infinite list of random Ints.

\subsection{Random Integers}
\begin{code}
randomInts :: Int -> Int -> [Int]
randomInts s1 s2 =
    if 1 <= s1 && s1 <= 2147483562 then
	if 1 <= s2 && s2 <= 2147483398 then
	    rands s1 s2
	else
	    error "randomInts: Bad second seed."
    else
	error "randomInts: Bad first seed."

rands :: Int -> Int -> [Int]
rands s1 s2 
   = if z < 1 then z + 2147483562 : rands s1'' s2'' 
     else 
	 z : rands s1'' s2''
     where	  
	k    = s1 `div` 53668
	s1'  = 40014 * (s1 - k * 53668) - k * 12211
	s1'' = if s1' < 0 then s1' + 2147483563 else s1'
    
	k'   = s2 `div` 52774
	s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
	s2'' = if s2' < 0 then s2' + 2147483399 else s2'

	z    = s1'' - s2''
\end{code}

\subsection{Random Doubles}

Same values for s1 and s2 as above, generates an infinite
list of Doubles uniformly distibuted in (0,1).

\begin{code}
randomDoubles :: Int -> Int -> [Double]
randomDoubles s1 s2 = map (\x -> (fromIntegral x * 4.6566130638969828e-10))
			  (randomInts s1 s2)
\end{code}

\section{Test Data}

\begin{code}
test1,test2,test3,test4,test5,test6,test7::[Int]
test1 = [1..10]
test2 = [10,9..1]
test3 = [1..500]
test4 = [500,499..1]

test5 = take 10   (randomInts 123213 342234)
test6 = take 100  (randomInts 123213 342234)
test7 = take 1000 (randomInts 123213 342234)
\end{code}

\section{Results}

\begin{rawlatex}
\begin{tabular}{||c||r|r||r|r||r|r||r|r||}
\hline 
& \multicolumn{2}{c||}{insertSort} &
  \multicolumn{2}{c||}{mergeSort} &
  \multicolumn{2}{c||}{quickSort} &
  \multicolumn{2}{c||}{lazySort} \\
\cline{2-9}
& \multicolumn{1}{c|}{red} &
  \multicolumn{1}{c||}{cell} &
  \multicolumn{1}{c|}{red} &
  \multicolumn{1}{c||}{cell} &
  \multicolumn{1}{c|}{red} &
  \multicolumn{1}{c||}{cell} &
  \multicolumn{1}{c|}{red} &
  \multicolumn{1}{c||}{cell}\\ 
\hline\hline
test1 	&	112 	&	225  	& 
		360	& 	636	& 
		362	& 	640	& 
		140 	& 	501 	\\
\hline
test2 	& 	184  	& 	368   	& 
		367	&	662	& 
		542	& 	910	& 
		140	& 	501	\\
\hline 
test3 	& 	5009 	& 	11406 	& 
		35309	& 	63196	& 
		753002  & 	1135146 &
		252000	&	883637	\\
\hline
test4	&	253517	&	507919	&
		35422	&	63590	&
		1252003	&	1883646	&
		252000	&	883637	\\
\hline 
test5	&	363	&	963	&
		373	&	768	&
		324	&	668	&
	  	367	&	1073	\\
\hline
test6	&	8366	&	19254	&
		6077	&	12273	&
		6002	&	10791	&
		4314	&	13514	\\
\hline 
test7	& 	519754	&	1065013	&
		84228	&	170230	&
		95620	&	161432	&
		25416	&	98789	\\
\hline 
\end{tabular}
\end{rawlatex}

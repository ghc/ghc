%\documentstyle[a4wIde,11pt]{report}
%\begin{document}
%\section{Lib.lgs --- Some miscellaneous functions}

%\begin{verbatim}

> module Lib where
> import Char(isDigit) -- 1.3
> import GoferPreludeBits

% 
%               Copyright (C) 1993  Medical Research Council
%
% SCCS: %W% %G%
%
%  MODIFICATIONS
%  ----------
% 18-01-94	 ipoole  added toDouble
% 08-11-93	 ipoole  added toFloat
% 08-11-93       derekc  added Coord2
% 04-08-93	 ipoole  now compiles with hbc and ghc-0.16.  added seq etc
% 18-08-93	 ipoole	 appended mrclib.lgs
% 09-06-93	 ipoole  fixed strToFloat "0.0" bug
% 11-03-93       derekc  added position, occurences, strToFloat, isDecimalFracn
% 11-03-93       derekc  strToInt, isNat, isInt <- IOapp.lgs,
%			 trace -> IOapp.lgs
% 02-03-93       derekc  newStyleIdentifiersUsed, put under sccs
% 23-02-03	 ipoole  deleted strToInt
% 14-02-93	 ipoole	 now no need for sqrt or truncate (use iSqrt)
% 08-11-92       ipoole  type coord moved in from mrclib, and added to
% 07-11-92       ipoole  added map2, numVal

%\end{verbatim}


\subsection{Miranda equivalents}
%------------------------------------------------------------------------------

\begin{Def}{hd, tail, map2, numVal}
These definitions are included to make the conversion of Miranda programs
into Gofer just a little easier. In general, prefer the Gofer forms.
\begin{vb}

> hd = head
> tl = tail
> map2 = zipWith
> numVal = strToInt    -- NB capitalised to meet SADLI coding standard

\end{verbatim}\end{vb}\end{Def}


\subsection{Standard Numerical Functions}
%------------------------------------------------------------------------------

\begin{Def}{absolute}~~\begin{vb}

> absolute :: Float -> Float
> absolute f | f < 0.0   = (-f)
>            | otherwise = f

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{strToFloat} 
A first attempt at converting strings to Floats. This cannot cope with
scientific notation, more than 10 significant digits or more than 9 decimal
places.  
\begin{vb}

> strToFloat :: String -> Float
> strToFloat "" = error "strToFloat (Null)"
> strToFloat str
>    | isDecimalFraction str = valAsInt / fromInt (10 ^ decimalPlaces str)
>    | otherwise             = error ("strToFloat: " ++ str)
>      where
>      	  valAsInt
>            | sigDigits str' >10 = error "strToFloat: >10 significant digits!"
>            | otherwise          = fromInt (strToInt str') :: Float
>         str'            =  filter (/='.') str
>	  sigDigits "0" = 1
>         sigDigits (ch:chs) | elem ch ['1'..'9'] =  1 + length chs
>                            | otherwise          =  sigDigits chs
>	  decimalPlaces str 
>            | pos  < 0   =  0
>	     | decs > 9   =  error "strToFloat: >9 decimal places!"
>            | otherwise  =  decs
>         decs            =  length str - pos - 1
>         pos             =  position '.' str

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{strToInt}
Turn a string containing only digits plus (optionally) leading spaces and 
minuses into an integer.
\begin{vb}

> strToInt :: String -> Int
> strToInt "" = error "strToInt (Null)"
> strToInt (' ':xs) = strToInt xs
> strToInt ('-':xs) = (negate . strToInt) xs
> strToInt xs
>       	= loop 0 xs where
>           loop n [] = n
>           loop n (' ':xs) = n
>           loop n (x:xs) | isDigit x = loop (10*n+(fromEnum x - fromEnum '0')) xs
>      	                  | otherwise = error ("strToInt: " ++ xs)

> toFloat :: Real a => a -> Float
> toFloat = fromRational . toRational

> toDouble :: Real a => a -> Double
> toDouble = fromRational . toRational

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{isInt}~~\begin{vb}

> isInt :: String -> Bool
> isInt [] = False
> isInt ('-':l) = isNat l
> isInt l = isNat l

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{isNat}~~\begin{vb}

> isNat :: String -> Bool
> isNat [] = False
> isNat l = all isDigit l

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{isDecimalFraction}~~\begin{vb}

> isDecimalFraction :: String -> Bool
> isDecimalFraction [] = False
> isDecimalFraction str = isInt str' && ((occurences '.' str) <= 1)
>   where str' = filter (/='.') str

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{iSqrt}~~\begin{vb}

> iSqrt :: Int -> Int
> iSqrt = truncate . (+ 0.5) . sqrt . fromInt

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{hugenum}~~\begin{vb}

> hugenum = 2147483647::Int  -- largest integer

\end{verbatim}\end{vb}\end{Def}


\subsection{Other general functions}
%------------------------------------------------------------------------------

\begin{Def}{tupled}~~\begin{vb}

> tupled :: (a -> b) -> (a, a) -> (b, b)
> tupled f (x, y) = (f x, f y)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{occurences}~~\begin{vb}

> occurences :: Eq a => a -> [a] -> Int
> occurences a [] = 0
> occurences a (a':as) 
>	     | a == a'   =  1 + occurences a as
>	     | otherwise =      occurences a as

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{position} 
Return the index of the given element in the list, or (-1)
if it is not present.
\begin{vb}

> position :: Eq a => a -> [a] -> Int
> position a as = posit 0 a as
>    where
>       posit n a [] = -1
>       posit n a (a':as)  | a==a'     =  n
>                          | otherwise =  posit (n+1) a as

\end{verbatim}\end{vb}\end{Def}

\subsection{Type Coord}
%------------------------------------------------------------------------------

\begin{Def}{Coord}~~\begin{vb}

> type Coord  = (Int,Int)
> type Coord2 = Coord

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{sqDistance}~~\begin{vb}

> sqDistance (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{scaleCoord}~~\begin{vb}

> scaleCoord :: Float -> Coord -> Coord
> scaleCoord s (x,y) = (round ((fromInt x) * s),
>                        round ((fromInt y) * s))

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{addCoord}~~\begin{vb}

> addCoord (x1,y1) (x2, y2) = (x1+x2, y1+y2)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{subCoord}~~\begin{vb}

> subCoord (x1,y1) (x2, y2) = (x1-x2, y1-y2)

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{relativeTo}~~\begin{vb}

> relativeTo (x',y') (x,y) = (x - x', y - y')

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{inside}
Is a point inside the rectangle with the given boxCorners?
\begin{vb}

> inside :: Coord -> (Coord,Coord) -> Bool        
> (x,y) `inside` ((blx,bly),(trx,try)) =
>           (blx <= x) && (x <= trx)  &&  (bly <= y) && (y <= try)

\end{verbatim}\end{vb}\end{Def}

#ifndef __GLASGOW_HASKELL__

\begin{Dec}{Coords are Nums}
Tuples are already members of Text, so nothing is needed to implement
Coord as a member of text (I think). But
let's make Coord an instance of class Num, in part at least:
\begin{vb}

> instance (Num a, Num b) => Num (a,b) where
>       (+) = addCoord
>       (-) = subCoord
>       negate (x,y) = (-x,-y)
> --    abs (x,y) = (abs x, abs y)            
> --    signum (x,y) = (signum x, signum y)

\end{verbatim}\end{vb}\end{Dec}

\begin{Def}{Coord3}
Coord3 will similarly come in handy:
\begin{vb}

> type Coord3 = (Int, Int, Int)

> instance (Num a, Num b, Num c) => Num (a,b,c) where
>       (x1,y1,z1) + (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
>       (x1,y1,z1) - (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)
>       negate (x,y,z) = (-x,-y,-z)
> --    abs (x,y,z) = (abs x, abs y, abs z)            
> --    signum (x,y,z) = (signum x, signum y, signum z)

\end{verbatim}\end{vb}\end{Def}

#endif __GLASGOW_HASKELL__

% Here to end was mrclib.lgs


\begin{Def}{sortBy} accepts a function and a list, and returns the list
ordered (ascending) according to the given function.  It can thus be used
on lists of structured types for which the \verb@'<'@ operator is not
valid, e.g. 
\begin{vb}

               sortBy fst [(4,"Fred"), (2,"Bert"), (6,"Gill")]
                      --> [(2,"Bert"), (4,"Fred"), (6,"Gill")]

> sortBy :: Ord b => (a->b) -> [a] -> [a]
> sortBy v [] = []
> sortBy v (a:x) 
>       = (sortBy v left) ++ [a] ++ (sortBy v right)
>         where
>         left  = [b | b <- x, (v b) <= va ]
>         right = [b | b <- x, (v b)  > va ]
>         va = v a

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{maxBy} returns the the element in the given list which yields the 
greatest value under the given function.
\begin{vb}

> maxBy :: Ord b => (a->b) -> [a] -> a
> maxBy f = foldl1 max2by
>               where max2by a b | (f a) >= (f b)  = a
>                                | otherwise       = b

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{minBy} similar to \verb@maxBy@
\begin{vb}

> minBy :: Ord b => (a->b) -> [a] -> a
> minBy f = foldl1 min2By
>               where min2By a b | (f a) <= (f b) = a
>                                | otherwise      = b

\begin{Def}{readTable} 
converts a text table of numbers (eg from a `feature file').
into [[Int]]
\begin{vb}

> readTable:: String -> [[Int]]
> readTable = map (map strToInt) . map words . lines

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{writeTable} the converse of readTable.
\begin{vb}

> writeTable:: Show{-was:Text-} a => [[a]] -> String
> writeTable = unlines . map unwords . (map . map) show

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{writeTableN} like readTable, but number each line.
\begin{vb}

> writeTableN:: Show{-was:Text-} a => [[a]] -> String
> writeTableN = layn . map unwords . (map . map) show

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{plotSurface} 
invokes the program ``surface'' to plot a 2--D surface via 
stoX. The \verb@switches@ parameter will be passed to ``surface'' 
(see "man l surface") and for a first try can be "".
\begin{vb}

> plotSurface :: String -> [[Int]] -> FailCont -> SuccCont -> Dialogue
> plotSurface switches table fail succ =
>   writeFile "surfacedata" surfData fail
>     (writeFile "plotsurf" surfCommand fail succ)
>     where
>       surfData = "Plotsurface" ++ "\n" ++
>                  show yLen ++ " " ++ show xLen ++ "\n" ++
>                  writeTable table
>       surfCommand = "cat surfacedata | surface " ++ switches ++ " | stoX\n"
>       xLen = length (table!!0)
>       yLen = length table

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{quadSolve} 
solve the quadratic equation $a x^2 + b x + c = 0$ for $x$, if
possible.  Both solutions are returned, in ascending order.  
Deals sensibly with $a=0$.
\begin{vb}

> quadSolve :: Float -> Float -> Float -> (Float, Float)
> quadSolve a b c
>    | a /= 0.0 && s1 > s2   =  (s1, s2)
>    | a /= 0.0 && s1 <= s2  =  (s2, s1) 
>    | otherwise           =  (-c/b, -c/b) 
>    where
>       s1 = (-b + root) / (2.0 * a)
>       s2 = (-b - root) / (2.0 * a)
>       bs4ac = b*b - 4.0*a*c
>       root | bs4ac >= 0.0  =  {-sqrt-} bs4ac 
>            | otherwise    
>                 = error ("quadSolve " ++ show [a,b,c] ++ " - no solution!") 

\end{verbatim}\end{vb}\end{Def}

\begin{Def}{number}
Here is a utility to check that a number is a (non negative) number.
\begin{vb}

> number :: String -> Bool
> number [] = False
> number [a] = isDigit a
> number (a:l) = isDigit a && (number l)

\end{verbatim}\end{vb}\end{Def}



\sectionHH{Some strict functions}

#ifdef Gofer

\begin{vb}

> seq :: a -> b -> b
> seq a b = strict (const b) a

> hyperSeq :: [a] -> b -> b
> hyperSeq as b = foldr seq b as

> hyperStrict :: ([a] -> b) -> ([a] -> b)
> hyperStrict f x = hyperSeq x (f x)

\end{verbatim}\end{vb}

#endif

%\end{document}

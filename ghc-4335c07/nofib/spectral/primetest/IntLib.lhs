\section{Some Integer Functions}
%$Log: IntLib.lhs,v $
%Revision 1.2  1996/07/25 21:32:53  partain
%Bulk of final changes for 2.01
%
%Revision 1.1  1996/01/08 20:04:20  partain
%Initial revision
%
%Revision 1.1  92/06/30  15:54:07  dlester
%Initial revision
%

In this module we define some useful functions on Integers.

> module IntLib (readInteger, showInteger, makeNumber, chop,
>                powerMod, cubeRoot, log2) where 
> import Data.List--1.3
> rcsid = "$Header: /srv/cvs/cvs.haskell.org/fptools/nofib/spectral/primetest/IntLib.lhs,v 1.2 1996/07/25 21:32:53 partain Exp $"

\subsection{Reading and Writing}

> readInteger :: String -> Integer
> readInteger s = read s

> showInteger :: Integer -> String
> showInteger i = show i

\subsection{Interconverting between bases}

We can make a large number from a list of numbers using @makeNumber@.
This satisfies:
\[@makeNumber@ \, @b@ \, [x_0,\,x_1,\,\ldots,\,x_n]
 = x_0.@b@^n + x_1.@b@^{n-1} + \cdots + x_n.@b@^0\]

> makeNumber :: Integer -> [Integer] -> Integer
> makeNumber b = foldl f 0 where f a x = a * b + x

The (left and right) inverse of @makeNumber@ is @chop@.

> chop :: Integer -> Integer -> [Integer]
> chop b = chop' [] where chop' a n = if n == 0 then a else chop' (r:a) q
>                                     where (q,r) = n `divMod` b

\subsection{Raising a number to a power}

The following function @powerMod@ calculates @a^b `mod` m@. I suspect
that this is the critical function in the benchmarking process, and
given that it can be computed {\em without} a great deal of extra
storage, it should be a candidate for being a built-in within the
Haskell library.

> powerMod :: Integer -> Integer -> Integer -> Integer
> powerMod a 0 m = 1
> powerMod a b m
>  = f a' (b-1) a'
>    where a' = a `mod` m
>          f a 0 c = c
>          f a b c = g a b where
>                    g a b | even b    = g ((a*a) `mod` m) (b `div` 2)
>                          | otherwise = f a (b-1) ((a*c) `mod` m)

[This coding of al-Kash\^{\i}'s algorithm is due to Joe Fasel.]

\subsection{Integer Cube roots}

The value $@y@=@cubeRoot x@$ is the integer cube root of @x@, {\it
i.e.} $@y@ = \lfloor \sqrt[3]{@x@} \, \rfloor$. Given $@x@\geq 0$,
@y@ satisfies the following conditions:
\[\begin{array}{lll}
@y@ &\geq & 0, \\
@y@^3 &\geq & @x@, \mbox{ and}\\
(@y@-1)^3 &<& @x@.
\end{array}\]
My implementation uses Newton's method.

> cubeRoot :: Integer -> Integer
> cubeRoot x = until satisfy improve x
>              where satisfy y = y*y*y >= x && y'*y'*y' < x where y' = y-1
>                    improve y = (2*y*y*y+x) `ddiv` (3*y*y)
>                    ddiv a b  = if (r < b `div` 2) then q else q+1
>                                where (q,r) = divMod a b

\subsection{Logarithm base 2}

The $@log2@ n$ is the @Integer@ $m$ such that $m = \lfloor\log_2
n\rfloor$.

> log2 :: Integer -> Integer
> log2 = genericLength . chop 2

This concludes the integer functions needed for the RSA algorithm.


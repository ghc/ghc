\begin{vb}

> module GoferPreludeBits where

This script contains some useful functions taken from the standard
Gofer prelude, but which do not appear in the Haskell prelude

SCCS: %W% %G%

> 
> copy             :: Int -> a -> [a]      -- make list of n copies of x
> copy n x          = take n xs where xs = x:xs


> cjustify, ljustify, rjustify :: Int -> String -> String

> cjustify n s = space halfm ++ s ++ space (m - halfm)
>                where m     = n - length s
>                      halfm = m `div` 2 
> ljustify n s = s ++ space (n - length s)
> rjustify n s = space (n - length s) ++ s

> space       :: Int -> String
> space n      = copy n ' '

> layn        :: [String] -> String
> layn         = lay (1::Int) where lay _ []     = []
>                                   lay n (x:xs) = rjustify 4 (show n) ++ ") "
>                                            ++ x ++ "\n" ++ lay (n+1) xs

> -- Merging and sorting lists:

> merge               :: Ord a => [a] -> [a] -> [a]
> merge []     ys      = ys
> merge xs     []      = xs
> merge (x:xs) (y:ys)
>         | x <= y     = x : merge xs (y:ys)
>         | otherwise  = y : merge (x:xs) ys

> sort                :: Ord a => [a] -> [a]
> sort                 = foldr insert []

> insert              :: Ord a => a -> [a] -> [a]
> insert x []          = [x]
> insert x (y:ys)
>         | x <= y     = x:y:ys
>         | otherwise  = y:insert x ys

> qsort               :: Ord a => [a] -> [a]
> qsort []             = []
> qsort (x:xs)         = qsort [ u | u<-xs, u<x ] ++
>                              [ x ] ++
>                        qsort [ u | u<-xs, u>=x ]

> --1.3: undefined = error "undefined"

\end{vb}



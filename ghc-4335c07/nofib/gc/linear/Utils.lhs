 
\section{Utils}

Here are some general functions not
specific to any one module.

\begin{code}
module Utils where

--import LMLTrace
{-
traceIndex :: String -> [a] -> Int -> a
traceIndex name lst i = trace name (lst!!i)
-}

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f x y = [f a b | (a,b) <- zip x y]

rep :: Int -> a -> [a]
rep n x = take n (repeat x)

rjustify :: Int -> [Char] -> [Char]
rjustify n s = spaces(n - (length s)) ++ s

spaces :: Int -> [Char]
spaces n = rep n ' '

zip2 :: [a] -> [b] -> [(a,b)]
zip2 x [] = []
zip2 [] y = []
zip2 (a:x) (b:y) = (a,b):zip2 x y
\end{code}





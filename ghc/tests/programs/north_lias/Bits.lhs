% Bits.lhs - useful extras for testing LIAS

> module Bits (
>     Cont(..),
>     showit, showits, new_line, pad
>     ) where

> type Cont  =  IO () --was: Dialogue

> showit :: (Show a) => a -> Cont -> Cont
> showit x c  =  putStr (show x) >> c

> showits :: String -> Cont -> Cont
> showits x c  =  putStr x >> c

> new_line :: Cont -> Cont
> new_line  =  showits "\n"

> pad :: Int -> String -> String
> pad n cs  =  take (n - length cs) (repeat ' ') ++ cs

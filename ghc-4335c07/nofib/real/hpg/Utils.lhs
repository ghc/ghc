% Utils.lhs - utility functions for the HPG

% @(#)Utils.lhs	1.20 dated 92/07/20 at 17:30:41

% Crown Copyright 1991

\section{Utility functions}

This module contains useful utility functions needed throughout the \HPG.
\begin{haskell}

> module Utils (
>     cmap, rep, print_str, split_str, finish
>     ) where

> import Data.Char
> import Config
> import Types
> import Env

\end{haskell}

\prog{cmap xcs xsc} applies \prog{xsc} to a list of values, being those
values which the \prog{xcs} supply to their continuations.
It is easier to read than to describe$\ldots$
\begin{verbatim}
cmap :: [(Xcont x) -> Cont] -> (Xscont x) -> Cont
cmap [] xsc        =  xsc []
cmap (xc:xcs) xsc  =  xc (\x -> cmap xcs (\xs -> xsc (x:xs)))
\end{verbatim}
We can transform the inner lambda abstraction by noting that:
\begin{verbatim}
(\xs -> xsc (x:xs)) == xsc . (:) x
\end{verbatim}
We can then transform the remaining lambda abstraction by noting that:
\begin{verbatim}
(\x -> cmap xcs (xsc . (:) x)) == cmap xcs . (.) xsc . (:)
\end{verbatim}
Thus the final version of \prog{cmap} is:
\begin{haskell}

> cmap :: [(Xcont x) -> Cont] -> (Xscont x) -> Cont
> cmap [] xsc        =  xsc []
> cmap (xc:xcs) xsc  =  xc (cmap xcs . (.) xsc . (:))

\end{haskell}

\prog{rep n x} is a list of \prog{n} copies of \prog{x}.
It is taken from the Miranda prelude.
\begin{haskell}

> rep :: Int -> x -> [x]
> rep n x  =  take n (repeat x)

\end{haskell}

\prog{print\_str s c} prints the string \prog{s} on the standard output and
then executes its continuation, \prog{c}.
\begin{haskell}

> print_str :: String -> Cont -> Cont
> print_str s c  =  get_output (\ op e -> op s >> c e)

\end{haskell}

\prog{split\_str n s} splits the string \prog{s} into lines by inserting
new line characters at whitespace, roughly every \prog{n}
characters.
By means of a considerable kludge, it takes account of any existing new
line characters in \prog{s}.
\begin{haskell}

> split_str :: Int -> String -> String
> split_str n ""  =  ""
> split_str n s   =  start ++ next ++ "\n"
>                    ++ split_str n (dropWhile isSpace rest)
>                    where
>                    (start, rest1)         =  split_num n s
>                    (next,  rest)          =  span (not . isSpace) rest1
>                    split_num 0 s          =  ("", s)
>                    split_num _ ""         =  ("", "")
>                    split_num _ ('\n':cs)  =  ("", ' ':cs)
>                    split_num m (c:cs)     =  (c:cs', cs'')
>                                              where
>                                              (cs',cs'') = split_num (m-one) cs

\end{haskell}

\prog{finish} is a continuation which does nothing.
\begin{haskell}

> finish :: Cont
> finish e  = return ()

\end{haskell}

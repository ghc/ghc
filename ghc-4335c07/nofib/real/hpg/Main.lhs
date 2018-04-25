% GenProg.lhs - program generation module for the HPG

% @(#)GenProg.lhs	1.20 dated 92/07/20 at 17:30:37

% Crown Copyright 1992

\section{Generating the program}
\label{genprog}

This module gathers the others together to generate and print the program.
\begin{haskell}

> module Main (
>     main
>     ) where

> import Config
> import Types
> import Env
> import Utils
> import GenType
> import GenVal
> import GenExp
> import System.Environment
> import System.IO

\end{haskell}

\prog{main} is the name of the main \HPG\ function.
The Haskell report requires that the entry point to the program executable
is called \prog{main} and is of type \prog{IO ()e}.
\begin{haskell}

> main :: IO ()
> main  =  do
>   argv <- getArgs
>   parse_args defaultArgs (unlines argv)

\end{haskell}

The data type \prog{Args} is used to package up the command line arguments
and give their types.
\begin{haskell}

> data Args  =  MkArgs (Int,Int,Int) Int Int Int Int Int String Output
>               deriving ()

\end{haskell}

The parameters to the program generator, and their command line flags, are:
\begin{enumerate}
\item A 3-tuple of seeds for the random number generator --- \prog{s}.
\item The number of type declarations to be generated and their
    depth --- \prog{nt} and \prog{dt} respectively.
\item The number of value declarations to be generated and their
    maximum depth --- \prog{nv} and \prog{dv} respectively.
\item The depth of the expressions to be generated --- \prog{de}.
\item A module name, \prog{mn}, for the output program.
\item A stream, \prog{op}, to which the program will be written.
\end{enumerate}

The default values of these parameters are given by \prog{defaultArgs}.
\begin{haskell}

> defaultArgs :: Args
> defaultArgs  =  MkArgs (9807,65,32975) 4 4 4 4 4 "Main" default_output

\end{haskell}

\prog{parse\_args} parses values passed to \prog{hpg} from the command line.
It is edited from output produced by \prog{mkhprog}, a command line parser
generator (see~\cite{north} for further details).
\begin{haskell}

> parse_args :: Args -> String -> IO ()
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'s':rest)
>     =  readval reads
>        (\val -> parse_args (MkArgs val x2 x3 x4 x5 x6 x7 x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'n':'t':rest)
>     =  readval reads
>        (\val -> parse_args (MkArgs x1 val x3 x4 x5 x6 x7 x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'d':'t':rest)
>     =  readval reads
>        (\val -> parse_args (MkArgs x1 x2 val x4 x5 x6 x7 x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'n':'v':rest)
>     =  readval reads
>        (\val -> parse_args (MkArgs x1 x2 x3 val x5 x6 x7 x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'d':'v':rest)
>     =  readval reads
>        (\val -> parse_args (MkArgs x1 x2 x3 x4 val x6 x7 x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'d':'e':rest)
>     =  readval reads
>        (\val -> parse_args (MkArgs x1 x2 x3 x4 x5 val x7 x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'m':rest)
>     =  readstring (\str -> parse_args (MkArgs x1 x2 x3 x4 x5 x6 str x8)) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'o':rest)
>     =  readstring (\str -> parse_args
>                         (MkArgs x1 x2 x3 x4 x5 x6 x7 (set_output str))) rest
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ""
>     =  hpg x1 x2 x3 x4 x5 x6 x7 x8
> parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) _
>     =  usage defaultArgs

\end{haskell}

\prog{hpg s nt dt nv dv de mn} generates a program with features as
described above.
It prints to the given output stream first a header giving the \HPG\ version
number and the supplied parameters, and then the generated program.
\begin{haskell}

> hpg :: (Int, Int, Int) -> Int -> Int -> Int -> Int -> Int -> String
>        -> Output -> Answer
> hpg s nt dt nv dv de mn op
>     =  print_str (head "")
>                  (gen_types max_vnts max_flds nt dt c1) (make_Env s op)
>        where
>        c1     =  gen_vals nv dv c2
>        c2     =  gen_exps de print_program
>        head   =  showString "-- HPG version " . version
>                  . newline
>                  . showString "-- Output from hpg "
>                  . sep_list space id
>                             [shows s, shows nt, shows dt, shows nv, shows dv,
>                              shows de]
>                  . newline
>                  . sep_list newline id [shead, thead, vhead, ehead]
>                  . newline . newline . mhead . newline . newline
>        shead  =  showString "-- Random number generator seeds: " . shows s
>        thead  =  showString "-- " . shows nt . showString " types, of depth "
>                  . shows dt
>        vhead  =  showString "-- " . shows nv . showString " values, of depth "
>                  . shows dv
>        ehead  =  showString "-- Expression depth: " . shows de
>        mhead  =  mod_name . space . showString mn . space . lbrack
>                  . main_name . rbrack . space . where_name . space . lbrace

\end{haskell}

\subsection{Printing programs}

This section deals with printing of the generated program.

\prog{print\_program vnes} prints a program consisting of the type
and value declarations in the environment, followed by a test of the
equivalence of the (value name, expression) pairs in \prog{vnes}.
\begin{haskell}

> print_program :: Xscont (Val_name, Expression)
> print_program vnes
>     =  get_all_type_decls (\tds -> get_all_val_decls (\vds ->
>            print_str (split_str line_len
>                ((sep_list dsep id (map showsType_decl tds
>                                    ++ map showsVal_decl vds)
>                 . dsep ) ""))
>                (print_test vnes)))
>        where
>        dsep  =  decl_sep . newline


\end{haskell}

\prog{print\_test vnes} prints a function, called \prog{main}, which prints
a list of \prog{bool}, one for each (value name, expression) pair
in \prog{vnes}.
The value of the \prog{bool} corresponding to \prog{(vn,e)} is
\prog{vn = e}.
\begin{haskell}

> print_test :: Xscont (Val_name, Expression)
> print_test vnes
>     =  print_str (split_str line_len
>                             ((main_name . val_def . showString print_name
>                               . space . lsq
>                               . sep_list list_separator showspair vnes . rsq
>                               . newline . rbrace)
>                              "")) finish
>        where
>        showspair (vn, e)  =  showString vn . space . showString eq_name
>                              . space . shows e

\end{haskell}

\subsection{Auxiliary functions}
This section contains the auxiliary functions used in parsing command
line arguments.
The functions are generated by \prog{mkhprog} (see~\cite{north} for
further details).

\prog{readstring} reads a string from the command line.
\begin{haskell}

> readstring :: (String -> String -> IO ()) -> String -> IO ()
> readstring f ""  =  f "" ""
> readstring f cs@(c:cs')
>     =  f s t
>        where
>        st      =  if c == '\n' then cs' else cs
>        (s,t1)  =  span ((/=) '\n') st
>        t       =  if t1 == "" then t1 else (tail t1)

\end{haskell}

\prog{readval} reads a value of arbitrary type from the command line.
It is used for reading integers and the random number generator seed
values.
\begin{haskell}

> readval :: (Read a) => ReadS a -> (a -> String -> IO ()) -> String
>                        -> IO ()
> readval readsfn f str
>     =  case thing of
>            []    -> usage defaultArgs
>            (_:_) -> f val (if str' == "" then str' else (tail str'))
>        where
>        thing        =  readsfn str
>        (val, str')  =  head thing

\end{haskell}

\prog{usage} is called if the command line contains invalid flags or values.
It prints a message giving a template for usage of \prog{hpg}.
\begin{haskell}

> usage :: Args -> IO ()
> usage (MkArgs s nt dt nv dv de mn _)
>     =  hPutStr stderr
>        ("Usage: hpg [-s (Int,Int,Int)] [-nt Int] [-dt Int] \
>                    \[-nv Int] [-dv Int] [-de Int] [-m String] [-o String]\n\
>         \    -s   random number generator seeds (default " ++ show s ++ ")\n\
>         \    -nt  number of types to be generated (" ++ show nt ++ ")\n\
>         \    -dt  depth of generated types (" ++ show dt ++ ")\n\
>         \    -nv  number of values to be generated (" ++ show nv ++ ")\n\
>         \    -dv  depth of values to be generated (" ++ show dv ++ ")\n\
>         \    -de  depth of expressions to be generated (" ++ show de ++ ")\n\
>         \    -m   output module name (" ++ mn ++ ")\n\
>         \    -o   output file name (stdout)\n")

\end{haskell}

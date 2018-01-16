% Env.lhs - environment for the Haskell Program Generator

% @(#)Env.lhs	1.20 dated 92/07/20 at 17:30:47

% Crown Copyright 1992

\section{Environment}

The environment module implements the environment used in generating Haskell
programs.
It is the repository of such useful data as random numbers, type and constructor
names, and information about types and values already generated.

This environment is changed as the \HPG\ progresses and is passed along
as a parameter to the functions comprising the \HPG; it does not appear
explicitly as a parameter due to the use of continuations.

This module
also defines the various types of continuation used and the result type
of the \prog{hpg} function itself.

The interface is as follows:
\begin{haskell}

> module Env (
>     Cont, Ncont, Econt, Vcont, Xcont, Xscont,
>     Answer,
>     Env, make_Env,
>     upto, choose, choosew,
>     Output, default_output,
>     get_constructors, get_val_names, get_type_names, get_all_type_names,
>     get_all_type_decls, get_all_val_decls, get_all_lambdas, get_type,
>     get_output,
>     push_lambda, pop_lambda, extend_type_env, extend_val_env, set_output
>     ) where

> import Config
> import Types
> import System.IO

\end{haskell}

\subsection{Types}
This module defines various sorts of continuation:
\begin{description}
\item[\prog{Cont}] the standard continuation --- takes an \prog{Env} and
    returns an \prog{Answer}.
\item[\prog{Xcont}] a continuation which takes an element of some type,
    \prog{x}, and returns a \prog{Cont}.
    The type \prog{Xcont} is parametrised by the type \prog{x}.
\item[\prog{Xscont}] a continuation which takes a list of elements of some type,
    \prog{x}, and returns a \prog{Cont}.
    The type \prog{Xscont} is parametrised by the type \prog{x}.
\item[\prog{Ncont}, \prog{Econt}, \prog{Vcont}] particular types of
    \prog{Xcont}, where the type \prog{x}
    is instantiated to \prog{Int}, \prog{Expression} and \prog{Value}
    respectively.
\end{description}
\begin{haskell}

> type Cont      =  Env -> Answer

> type Xcont x   =  x -> Cont
> type Xscont x  =  [x] -> Cont

> type Ncont     =  Xcont Int
> type Econt     =  Xcont Expression
> type Vcont     =  Xcont Value

\end{haskell}

As the Haskell Program Generator produces output, its result must be of
type \prog{Dialogue}, so \prog{Answer} is just a type synonym:
\begin{haskell}

> type Answer    =  IO () --Dialogue

\end{haskell}

\subsection{Random numbers}
This section gives a source of random numbers, to be carried around
in the environment, and some interface functions for accessing the
random numbers.

The random number generator is taken from Wichmann and Hill's
paper~\cite{wichmann.random}.
It requires three seeds to initialise it, which must be supplied to the
initial environment, and it gives an infinite list of pseudo-random
numbers.
These numbers are then removed from the list as required by the interface
functions below.

The \prog{fromIntegral}s in \prog{combine} ensure that the \prog{Int}s are
converted to \prog{Floats} before calculation.
\begin{haskell}

> random_numbers :: (Int, Int, Int) -> [Float]
> random_numbers (s1,s2,s3)
>     =  map (snd . properFraction . combine) (iterate f (s1,s2,s3))
>        where
>        combine :: (Int,Int,Int) -> Float
>        combine (a,b,c)  =
>             fromIntegral(a)/30269 + fromIntegral(b)/30307
>             + fromIntegral(c)/30323
>        f (a,b,c)  =
>            ((171*a) `mod` 30269, (172*b) `mod` 30307, (170*c) `mod` 30323)

\end{haskell}

\prog{upto n nc} applies \prog{nc} to an integer in the range
$0 \ldots n$ inclusive.
The convoluted definition of \prog{x} does a floating point multiplication of
\prog{r} by \prog{n+1}, takes the \prog{Integer} part and converts it to an
\prog{Int}.

The random number used is printed on the standard output, partly to aid in
debugging, and partly because the printing prevents the manifestation of
a bug in one of the current Haskell compilers.
If the \prog{appendChan} line is replaced by the one above it, the printing
will not occur.
% NB - mkTEnv relies on the layout of upto, so be careful in changing it.
\begin{haskell} 
 
> upto :: Int -> Ncont -> Cont
> upto n nc (MkEnv (r:rs) cs ts vs te ve le op)
>     =  hPutStr stderr (show x ++ " ") >> nc x (MkEnv rs cs ts vs te ve le op)
> --  =  appendChan stderr (show x ++ " ") exit ((nc x) (MkEnv rs cs ts vs te ve le op))
>        where
>        x :: Int
>        x  =  fromIntegral (fst (properFraction (r * (fromIntegral (n+one)))))
 
\end{haskell}
 
\prog{choose xs xc} applies \prog{xc} to a random element of the list
\prog{xs}.
\begin{haskell}
 
> choose :: [x] -> (Xcont x) -> Cont
> choose xs xc  =  upto (length xs - one) (\n -> xc (xs !! n))

\end{haskell}

\prog{choosew ps xc} applies \prog{xc} to an item chosen from the list
\prog{ps} as follows: each element of \prog{ps} is a pair of an integer
and an item; the integer determines the probability of choosing the item
as the integer divided by the sum of the integers in the list.
(\prog{choosew} means ``weighted choice''.)
\begin{haskell}

> choosew :: [(Int,x)] -> (Xcont x) -> Cont
> choosew ps xc
>     =  upto (sum [i | (i,x) <- ps] - one) (f ps)
>        where
>        f ((i,x):ps') n | n < i  =  xc x
>                        | True   =  f ps' (n-i)

\end{haskell}

\subsection{Names}
The \HPG\ requires sources of names for types, values and constructors.
These are stored in the environment and are just a string consisting
of an identifying name and a number:
\begin{haskell}

> idnum :: String -> [String]
> idnum id  =  [id ++ show n | n <- [one..]]

> constructors :: [Constructor]
> constructors  =  idnum constructor_name

> type_names :: [Type_name]
> type_names    =  idnum type_name

> val_names :: [Val_name]
> val_names     =  idnum val_name

\end{haskell}

\subsection{Type, value and lambda environments}
The type and value environments are used to store declarations of previously
generated types and values respectively.
The lambda environment stores the names and values of lambda-bound variables.
\begin{haskell}

> type Type_env  =  [Type_decl]

> initial_type_env :: Type_env
> initial_type_env  =  []

> type Val_env  =  [Val_decl]

> initial_val_env :: Val_env
> initial_val_env  =  []

> type Lambda_env  =  [(Val_name, Value)]

> initial_lambda_env :: Lambda_env
> initial_lambda_env  =  []

\end{haskell}

\subsection{The output stream}
The \HPG\ writes its output program to the standard output by default, but
it can be given a file name to which output is written instead.
The output stream is carried as part of the environment as an element of
type \prog{Output}, defined as:
\begin{haskell}

> --type Output  =  String -> FailCont -> SuccCont -> Dialogue
> type Output  =  String -> IO ()

\end{haskell}
The default value is:
\begin{haskell}

> default_output :: Output
> default_output  str =  putStr str

\end{haskell}

\prog{set\_output str} returns a value of \prog{Output} in which the
output stream is the file whose name is \prog{str}.
\begin{haskell}

> set_output :: String -> Output
> set_output str  =  appendFile str

\end{haskell}

\subsection{Environment construction}
The \prog{Env} type is just a collection of all the environment information,
bound together with a constructor.
\begin{haskell}

> data Env  =  MkEnv [Float] [Constructor] [Type_name] [Val_name]
>                    Type_env Val_env Lambda_env Output

\end{haskell}

\prog{make\_Env} returns an environment, constructed from its parameters.
% NB - mkTEnv relies on the layout of make_Env, so be careful in changing it.
\begin{haskell}

> make_Env :: (Int,Int,Int) -> Output -> Env
> make_Env (s1,s2,s3) op  =  MkEnv (random_numbers (s1,s2,s3)) 
>                               constructors type_names val_names
>                               initial_type_env initial_val_env
>                               initial_lambda_env op

\end{haskell}

\subsection{Environment extraction functions}
These functions use the environment as a source of random numbers or
names, applying their continuation to the data extracted and
an altered environment from which the data has been removed, if necessary.
\begin{haskell}

> get_constructors :: Int -> (Xscont Constructor) -> Cont
> get_constructors n csc (MkEnv rs cs tns vns te ve le op)
>     =  csc (take n cs) (MkEnv rs (drop n cs) tns vns te ve le op)

> get_val_names :: Int -> (Xscont Val_name) -> Cont
> get_val_names n vnsc (MkEnv rs cs tns vns te ve le op)
>     =  vnsc (take n vns) (MkEnv rs cs tns (drop n vns) te ve le op)

> get_type_names :: Int -> (Xscont Type_name) -> Cont
> get_type_names n tnsc (MkEnv rs cs tns vns te ve le op)
>     =  tnsc (take n tns) (MkEnv rs cs (drop n tns) vns te ve le op)

> get_all_type_names :: (Xscont Type_name) -> Cont
> get_all_type_names tnsc e@(MkEnv _ _ _ _ te _ _ _)
>     =  tnsc [tn | (tn,t) <- te] e

> get_all_type_decls :: (Xscont Type_decl) -> Cont
> get_all_type_decls tdsc e@(MkEnv _ _ _ _ te _ _ _)
>     = tdsc te e

> get_all_val_decls :: (Xscont Val_decl) -> Cont
> get_all_val_decls vdsc e@(MkEnv _ _ _ _ _ ve _ _)
>     = vdsc ve e

> get_all_lambdas :: (Xcont Lambda_env) -> Cont
> get_all_lambdas lec e@(MkEnv _ _ _ _ _ _ le _)
>     =  lec le e

> get_type :: Type_name -> (Xcont Htype) -> Cont
> get_type tn tc e@(MkEnv _ _ _ _ te _ _ _)
>     =  tc (head [t | (tn',t) <- te , tn == tn']) e

> get_output :: (Xcont Output) -> Cont
> get_output oc e@(MkEnv _ _ _ _ _ _ _ op)  =  oc op e

\end{haskell}

\subsection{Extension functions}
\prog{extend\_type\_env tds c} applies \prog{c} to an environment extended
with the list of type declarations \prog{tds}.
\begin{haskell}
 
> extend_type_env :: [Type_decl] -> Cont -> Cont
> extend_type_env tds c (MkEnv rs cs tns vns te ve le op)
>     =  c (MkEnv rs cs tns vns (tds++te) ve le op)
 
\end{haskell}
 
\prog{extend\_val\_env vds c} applies \prog{c} to an environment extended
with the list of value declarations \prog{vds}.
\begin{haskell}
 
> extend_val_env :: [Val_decl] -> Cont -> Cont
> extend_val_env vds c (MkEnv rs cs tns vns te ve le op)
>     =  c (MkEnv rs cs tns vns te (vds++ve) le op)
 
\end{haskell}

\subsection{Lambda environment functions}
\prog{push\_lambda (vn,v) c} adds the \prog{Val\_name}, \prog{Value} pair
\prog{(vn,v)} to the front of the lambda environment list and applies
\prog{c} to the resulting environment.
\begin{haskell}

> push_lambda :: (Val_name, Value) -> Cont -> Cont
> push_lambda (vn,v) c (MkEnv rs cs tns vns te ve le op)
>     =  c (MkEnv rs cs tns vns te ve ((vn,v):le) op)

\end{haskell}

\prog{pop\_lambda lc} removes the front item from the lambda environment
list and applies \prog{lc} to this and resulting environment.
It is an error for the lambda environment to be empty.
\begin{haskell}

> pop_lambda :: Xcont (Val_name,Value) -> Cont
> pop_lambda lc (MkEnv rs cs tns vns te ve ((vn,v):le) op)
>     =  lc (vn,v) (MkEnv rs cs tns vns te ve le op)

\end{haskell}

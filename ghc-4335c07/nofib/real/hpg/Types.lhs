% Types.lhs - type definitions for the Haskell Program Generator

% @(#)Types.lhs	1.20 dated 92/07/20 at 17:30:44

% Crown Copyright 1992

\section{Types}

This module declares most of the types used in the \HPG.
These are the types used to represent Haskell types, type declarations,
values, value declarations and expressions.

All constructed types derive \prog{Eq} --- this is not strictly necessary,
but is useful for testing.
They are also instances of \prog{Text} --- this allows us to print them
out when needed.
Notice, however, that there is no \prog{readsPrec} function as we do not
need to read any of them.

\begin{haskell}

> module Types (
>     Type_decl, Htype, Argtype(..), Base_type(..), Num_kind(..),
>     Constructor, Type_name, Val_decl, Value(..), Val_name,
>     Expression(..),
>     showsType_decl, showsVal_decl, vrange,
>     int_val,
>     sep_list
>     ) where

> import Data.Char (isAlpha) -- 1.3
> import Config

\end{haskell}

\subsection{Types}

The types in this section are used to represent Haskell types and
type declarations.
Function types are not yet implemented.

A type declaration (\prog{Type\_decl}) is a pair of a type name
(\prog{Type\_name}) and a Haskell type (\prog{Htype}).
\begin{haskell}

> type Type_decl    =  (Type_name, Htype)

\end{haskell}
\prog{Htype} represents algebraic types and is a list of pairs, where
the first element is a constructor name and the second element is a
list of \prog{Argtype}s, with as many elements as the arity of the
constructor.
\begin{haskell}

> type Htype        =  [(Constructor, [Argtype])]

\end{haskell}
An \prog{Argtype} is just a type name, a built-in type, a list of some type,
a tuple type or an array type.
\begin{haskell}

> data Argtype      =  Name_type  Type_name
>                   |  Basic_type Base_type
>                   |  List_type  Argtype
>                   |  Tuple_type [Argtype]
>                   |  Array_type Argtype Argtype
>      deriving (Eq)

> data Base_type    =  Num_type Num_kind | Bool_type | Char_type
>      deriving (Eq)

> data Num_kind     =  Int_type | Integer_type | Float_type | Double_type
>      deriving (Eq)

> type Constructor  =  String
> type Type_name    =  String

\end{haskell}
Note that \prog{Num\_kind} can only represent a subset of the numeric
types in Haskell.
These could easily be extended.

We declare \prog{Argtype}, \prog{Base\_type} and \prog{Num\_kind}
as instances of
\prog{Text} for later use in printing the generated program.
\begin{haskell}

> instance Show Argtype where
>     showsPrec _ (Name_type tn)    =  showString tn
>     showsPrec _ (Basic_type bt)   =  shows bt
>     showsPrec _ (List_type at)    =  lsq . shows at . rsq
>     showsPrec _ (Tuple_type ats)
>         =  lbrack . sep_list list_separator shows ats . rbrack
>     showsPrec d (Array_type at at')
>         =  showParen (d >= apply_prec)
>            (array_type_name . space . showsPrec apply_prec at
>             . space . showsPrec apply_prec at')

> instance Show Base_type where
>     showsPrec _ (Num_type k)  =  shows k
>     showsPrec _ Bool_type     =  bool_type_name
>     showsPrec _ Char_type     =  char_type_name

> instance Show Num_kind where
>     showsPrec _ Int_type      =  int_type_name
>     showsPrec _ Integer_type  =  integer_type_name
>     showsPrec _ Float_type    =  float_type_name
>     showsPrec _ Double_type   =  double_type_name

\end{haskell}

We also give a display function for \prog{Type\_decl}s.
\begin{haskell}

> showsType_decl :: Type_decl -> ShowS
> showsType_decl (tn, ht)
>     =  data_name . space . showString tn . type_def
>        . sep_list union showsVnt ht . space . derive_eq
>        where
>        showsVnt (c, ats)  =  showString c . field_separator
>                              . sep_list field_separator
>                                         (showsPrec apply_prec) ats

\end{haskell}

\subsection{Values}
Elements of the types in this section are used to represent values of the
types represented the previous section.

A value declaration is a pair of a value name and its value.
Notice that values do not come with an associated type --- the type of
a value could be included, but it is never necessary, though we can think
of extensions to the \HPG\ where it might be useful.
\begin{haskell}

> type Val_decl  =  (Val_name, Value)

\end{haskell}
The structure of \prog{Values} is much like that of \prog{Argtype}s, but
notice that all numeric values are \prog{Int}s.
See section~\ref{numbers} for a note on numeric values in the \HPG).
\begin{haskell}

> data Value     =  Num_val    Num_kind Int
>                |  Bool_val   Bool
>                |  Char_val   Char
>                |  List_val   [Value]
>                |  Tuple_val  [Value]
>                |  Tagged_val Constructor [Value]
>                |  Array_val  (Value,Value) [(Value, Value)]
>      deriving (Eq)

> type Val_name  =  String

\end{haskell}

We declare \prog{Value} as an instance of \prog{Text} for later use in
printing the generated program.
\begin{haskell}

> instance Show Value where
>     showsPrec d (Num_val Int_type n)      =  showsPrec d n
>     showsPrec d (Num_val Integer_type n)  =  showsPrec d n
>     showsPrec d (Num_val Float_type n)    =  showsPrec d (int_to_float n)
>     showsPrec d (Num_val Double_type n)   =  showsPrec d (int_to_double n)
>     showsPrec d (Bool_val b)              =  showsPrec d b
>     showsPrec d (Char_val c)              =  showsPrec d c
>     showsPrec _ (List_val vs)
>         =  lsq . sep_list list_separator shows vs . rsq
>     showsPrec _ (Tuple_val vs)
>         =  lbrack . sep_list list_separator shows vs . rbrack
>     showsPrec d (Tagged_val c vs)
>         =  showParen (d >= apply_prec)
>            (showString c . field_separator
>            . sep_list field_separator (showsPrec apply_prec) vs)
>     showsPrec d (Array_val (v,v') avs)
>         =  showParen (d >= apply_prec)
>            (array_name . space . (showsPrec apply_prec (Tuple_val [v,v']))
>             . space . showsPrec apply_prec avs)

\end{haskell}

We also give a display function for \prog{Val\_decl}s.
\begin{haskell}

> showsVal_decl :: Val_decl -> ShowS
> showsVal_decl (vn, v)  =  showString vn . val_def . shows v

\end{haskell}

It becomes convenient when generating array values to have a function which
gives the equivalent of the standard Haskell
function \prog{range} on \prog{Value}s.
\prog{vrange} is only validly applicable to basic values or tuples constructed
from values to which vrange is applicable, and gives an error
if applied to anything else.
\begin{haskell}

> vrange :: (Value,Value) -> [Value]
> vrange (Num_val Int_type n, Num_val Int_type n')
>     =  [int_val r | r <- [n..n']]
> vrange (Num_val Integer_type n, Num_val Integer_type n')
>     =  [Num_val Integer_type r | r <- [n..n']]
> vrange (Bool_val b, Bool_val b')      =  [Bool_val r | r <- [b..b']]
> vrange (Char_val c, Char_val c')      =  [Char_val r | r <- [c..c']]
> vrange (Tuple_val vs, Tuple_val vs')  =
>     [Tuple_val vs'' | vs'' <- f (map vrange (zip vs vs'))]
>     where
>     f []        =  [[]]
>     f (vs:vss)  =  [(i:is) | i <- vs, is <- f vss]
> vrange x                              =  error ("Error: vrange called on "
>                                                 ++ show x)

\end{haskell}

\subsubsection{Note on numeric values}
\label{numbers}
Numeric values are represented in the form:
\begin{verbatim}
Num_val Num_kind Int
\end{verbatim}
The numeric value is an integer, while the type to which it belongs is
indicated by the \prog{Num\_kind} value.
The \HPG\ only deals with those subsets of \prog{Integer},
\prog{Float} and \prog{Double} which correspond to \prog{Int}s.

All arithmetic in the \HPG\ is therefore on \prog{Int}s, and those
representing \prog{Integer}s, \prog{Float}s or \prog{Double}s
are shown as such, if necessary, on output.
Thus, for example, the floating point number $4.0$ is represented as
\begin{verbatim}
Num_val Float_type 4
\end{verbatim}
While the floating point number $4.6$ will never be generated by the \HPG.

\prog{int\_to\_float n} and \prog{int\_to\_double n} return the values
representing the floating point or double precision
number respectively, corresponding to the \prog{Int}, \prog{n}.
\begin{haskell}

> int_to_float :: Int -> Float
> int_to_float n  =  fromIntegral n

> int_to_double :: Int -> Double
> int_to_double n  =  fromIntegral n

\end{haskell}

\prog{int\_val n} returns the value representing to the \prog{Int}, \prog{n}.
\begin{haskell}

> int_val :: Int -> Value
> int_val n  =  Num_val Int_type n

\end{haskell}

\subsection{Expressions}
Elements of the types in this section represent expressions which should
evaluate to the values represented by elements of the types in the previous
section.

An expression can be one expression applied to another, an identifier,
a value, a constructor applied to the number of expressions given by its
arity, a list, a tuple, a lambda expression or an array.
\begin{haskell}

> data Expression  =  Apply_exp  Expression Expression
>                  |  Id_exp     Val_name
>                  |  Val_exp    Value
>                  |  Tagged_exp Constructor [Expression]
>                  |  List_exp   [Expression]
>                  |  Tuple_exp  [Expression]
>                  |  Lambda_exp Val_name Expression
>                  |  Array_exp  Expression Expression
>      deriving (Eq)

\end{haskell}

We declare \prog{Expression} as an instance of \prog{Text} for later use in
printing the generated program.
\begin{haskell}

> instance Show Expression where
>     showsPrec d (Apply_exp e1 e2)
>         =  showParen (d > apply_prec)
>            (showsPrec apply_prec e1 . space . showsPrec (apply_prec+one) e2)
>     showsPrec _ (Id_exp vn)
>         =  showParen (is_op vn) (showString vn)
>            where
>            is_op (c:cs)  =  not (isAlpha c)
>     showsPrec d (Val_exp v)        =  showsPrec d v
>     showsPrec d (Tagged_exp c es)
>         =  showParen (d > apply_prec)
>            (showString c . field_separator
>             . sep_list field_separator (showsPrec (apply_prec+one)) es)
>     showsPrec _ (List_exp es)
>         =  lsq . sep_list list_separator shows es . rsq
>     showsPrec _ (Tuple_exp es)
>         =  lbrack . sep_list list_separator shows es . rbrack
>     showsPrec _ (Lambda_exp vn e)
>         =  lbrack . lambda_name . showString vn . space . map_name . space
>            . shows e . rbrack
>     showsPrec d (Array_exp e e')
>         =  showParen (d > apply_prec)
>            (array_name . space . showsPrec (apply_prec+one) e . space
>             . showsPrec (apply_prec+one) e')

\end{haskell}

\subsection{Utility printing functions}
We define here some useful functions for printing types and other parts
of a generated program.
These functions properly belong in \prog{Utils}, but that would give
a cyclic dependency between \prog{Types}, \prog{Utils} and \prog{Env}.

\prog{sep\_list s xs} prints the list \prog{xs} by applying the printing
function \prog{f} to each element and separating the resulting list
by the \prog{ShowS} function \prog{s}.
\begin{haskell}

> sep_list :: ShowS -> (a -> ShowS) -> [a] -> ShowS
> sep_list s f []  =  empty_string
> sep_list s f xs  =  foldr1 g (map f xs)
>                     where
>                     g x y  =  x . s . y

\end{haskell}

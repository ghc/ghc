% GenType.lhs - type generation functions for the HPG

% @(#)GenType.lhs	1.20 dated 92/07/20 at 17:30:45

% Crown Copyright 1992

\section{Type generation}

This section describes the generation of random algebraic datatypes.
The complexity of the datatypes is determined by the following
parameters:
\begin{description}
\item[Variants] the number of alternatives in a datatype declaration.
    For example:
    \begin{verbatim}
    data Tree = Leaf Int | Node Tree Tree 
    \end{verbatim}
    has two variants.
\item[Fields] the arity of a constructor.
    In the example above, \prog{Leaf} has one field and \prog{Node} has
    two fields.
\item[Depth] describes the complexity of the type in a field.
    A basic type (\prog{Int}, \prog{Bool} etc) has depth 1, while a list
    tuple or array has depth one greater than that of its arguments.
\end{description}
These parameters are all given as arguments to the \HPG.
One further parameter is compiled into the \HPG: \prog{max\_tuple\_len}
determines the maximum number of elements in a tuple.
Most Haskell implementations have a fairly small upper limit on this
figure (such as 5), so it does not seem worth allowing the user to specify
it.

One more point to notice is that the first variant of every datatype
generated has no fields.
This ensures that generation of values is always possible, as every type
is guaranteed to have at least one finite value.
If this was not the case, we might generate type declarations such as:
\begin{verbatim}
data Weird = Slightly Weird | Mighty Weird
\end{verbatim}
which has no finite values.

\begin{haskell}

> module GenType (
>     gen_types
>     ) where

> import Utils
> import Config
> import Types
> import Env

\end{haskell}

\prog{gen\_types vnts flds n dp c} applies \prog{c} to a an environment
extended with \prog{n}
\prog{(Type\_name, Htype)} pairs, with each \prog{Htype} having depth
\prog{dp} and at most
\prog{vnts} variants, each with at most \prog{flds} fields.
At least one variant of each \prog{Htype} is a zero-arity constructor.
\begin{haskell}

> gen_types :: Int -> Int -> Int -> Int -> Cont -> Cont
> gen_types vnts flds n dp c
>     =  get_type_names n
>        (\tns -> get_all_type_names
>        (\tns' -> cmap (rep n (gen_type vnts flds (tns'++tns) dp))
>        (\ts -> extend_type_env (zip tns ts) c)))

\end{haskell}

\prog{gen\_type vnts flds tns dp tc} applies \prog{tc} to a type of depth
\prog{dp} which may refer to other types in the type name list \prog{tns}.
The first variant of this type is a zero-arity constructor.
\begin{haskell}

> gen_type :: Int -> Int -> [Type_name] -> Int -> Xcont Htype -> Cont
> gen_type vnts flds tns dp tc
>     =  upto (vnts-one)
>        (\n -> get_constructors (n + one)
>        (\cs -> cmap (rep n (gen_argtypes flds tns dp))
>        (\as -> tc (zip cs ([]:as)))))
 
\end{haskell}
 
\prog{gen\_argtypes flds tns dp asc} applies \prog{asc} to a list of up to
\prog{flds} argtypes, each of depth \prog{dp}, possibly referring to names
in \prog{tns}.
\begin{haskell}
 
> gen_argtypes :: Int -> [Type_name] -> Int -> Xscont Argtype -> Cont
> gen_argtypes flds tns dp asc
>     =  upto flds (\n -> cmap (rep n (gen_argtype tns dp)) asc)
 
\end{haskell}
 
\prog{gen\_argtype tns dp ac} applies \prog{ac} to a random argtype, of
depth \prog{dp}, which may refer to names in \prog{tns}.
It operates by selecting one of the elements of the list argument of choose;
this generates either a type name, a basic type, a list type, a tuple type
or an array type.
\begin{haskell}
 
> gen_argtype :: [Type_name] -> Int -> Xcont Argtype -> Cont
> gen_argtype tns dp ac | dp <= one  =  gen_base ac
> gen_argtype tns dp ac
>     =  choose [upto (length tns - one) (\n -> ac (Name_type (tns !! n))),
>                gen_base ac,
>                gen_argtype tns (dp - one) (\a -> ac (List_type a)),
>                upto max_tuple_len
>                  (\r -> cmap (rep r (gen_argtype tns (dp - one)))
>                  (\as -> ac (Tuple_type as))),
>                gen_ix_type tns (dp - one)
>                  (\a -> gen_argtype tns (dp - one)
>                  (\a' -> ac (Array_type a a')))] id
>                
 
\end{haskell}
Note that we can generate tuple types with just one component --- these are
degenerate and are interpreted as the component type, without any
``tupleness''.
We can also generate tuples with no elements --- this is perfectly valid
Haskell and is the type with just one element, written \prog{()}.

\prog{gen\_base ac} applies \prog{ac} to a random argtype, consisting
of a basic type.
This is surprisingly convoluted, and there may well be a better way.
\begin{haskell}

> gen_base :: Xcont Argtype -> Cont
> gen_base ac
>     =  choose [choose [Int_type, Integer_type, Float_type, Double_type]
>                       (\k -> ac (Basic_type (Num_type k))),
>                ac (Basic_type Bool_type),
>                ac (Basic_type Char_type)] id

\end{haskell}

\prog{gen\_ix\_type tns dp ac} applies \prog{ac} to an argtype of class
\prog{Ix}, of depth \prog{dp}, which may refer to names in \prog{tns}.
We need argtypes of class \prog{Ix} for indexing of arrays.
\begin{haskell}

> gen_ix_type :: [Type_name] -> Int -> Xcont Argtype -> Cont
> gen_ix_type tns dp ac | dp <= one  =  gen_ix_base ac
> gen_ix_type tns dp ac
>     =  choose [gen_ix_base ac,
>                upto max_tuple_len
>                  (\r -> cmap (rep r (gen_ix_type tns (dp-one)))
>                  (\as -> ac (Tuple_type as)))] id

\end{haskell}

\prog{gen\_ix\_base ac} applies \prog{ac} to a random argtype, consisting
of a basic type of class \prog{Ix}.
\begin{haskell}

> gen_ix_base :: Xcont Argtype -> Cont
> gen_ix_base ac
>     =  choose [choose [Int_type, Integer_type]
>                       (\k -> ac (Basic_type (Num_type k))),
>                ac (Basic_type Bool_type),
>                ac (Basic_type Char_type)] id

\end{haskell}

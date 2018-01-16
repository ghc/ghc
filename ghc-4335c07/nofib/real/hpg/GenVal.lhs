% GenVal.lhs - generation of random values for the HPG

% @(#)GenVal.lhs	1.20 dated 92/07/20 at 17:30:40

% Crown Copyright 1992

\section{Value generation}

This section describes generation of random values for the \HPG.

The complexity of a random value is determined by a depth parameter:
a zero-arity constructor has depth one, while a non-zero-arity
constructor has depth one greater than its deepest argument.
Lists, tuples and arrays also have depth one greater than their
deepest component.

The main interface to this module is \prog{gen\_vals}, but we also
export \prog{gen\_id\_val}, as it is used in generating lambda
expressions (see section~\ref{exp.lambda}).
\begin{haskell}

> module GenVal (
>     gen_vals, gen_id_val
>     ) where

> import Utils
> import Config
> import Types
> import Env

\end{haskell}

\prog{gen\_vals n dp c} extends the current value environment
with \prog{n} new value declarations of depth $\leq$ \prog{dp} (which
must be $\geq 1$).
\begin{haskell}

> gen_vals :: Int -> Int -> Cont -> Cont
> gen_vals n dp c
>     =  get_all_type_names
>        (\tns -> cmap (rep n (upto (length tns - one)
>                 . (\vc r -> gen_val (tns !! r) dp vc)))
>        (\vs -> get_val_names n
>        (\vns -> extend_val_env (zip vns vs) c)))

\end{haskell}

\prog{gen\_val tn dp c} applies \prog{c} to a value of type \prog{tn}
of depth $\leq$ \prog{dp}.
\begin{haskell}

> gen_val :: Type_name -> Int -> Vcont -> Cont
> gen_val tn dp vc
>     | dp <= one =  get_type tn (\t -> vc (Tagged_val (fst (head t)) []))
>     | otherwise =  get_type tn tc
>                    where
>                    tc t  =  upto (length t - one) nc
>                             where
>                             nc r  =  cmap (map (gen_id_val (dp - one)) ids)
>                                      (\vs -> vc (Tagged_val c' vs))
>                                      where
>                                      (c', ids)  =  t !! r

\end{haskell}

\prog{gen\_id\_val dp id c} applies \prog{c} to a value of the given
\prog{Argtype}, of depth $\leq$ \prog{dp}.
\begin{haskell}

> gen_id_val :: Int -> Argtype -> Vcont -> Cont
> gen_id_val dp (Name_type tn) vc  =  gen_val tn dp vc
> gen_id_val dp (Basic_type bt) vc  =  gen_basic_val bt vc
> gen_id_val dp (List_type id) vc
>     =  upto max_list_len
>        (\r -> cmap (rep r (gen_id_val (dp - one) id))
>        (\vs -> vc (List_val vs)))
> gen_id_val dp (Tuple_type ids) vc
>     =  cmap (map (gen_id_val (dp - one)) ids)
>        (\vs -> vc (Tuple_val vs))
> gen_id_val dp (Array_type id id') vc
>     =  let vsc [v,v']
>                =  upto (max_array_len-one)
>                   (\r -> let vsc' vs  =  vc (Array_val (v,v'')
>                                                 (zipWith (,) vrng vs))
>                              vrng     =  take (r+one) (vrange (v,v'))
>                              v''      =  case vrng of
>                                              []    -> v'
>                                              (_:_) -> last vrng
>                          in
>                              cmap (rep (length vrng)
>                                   (gen_id_val (dp-one) id')) vsc')
>        in
>            cmap (rep 2 (gen_id_val (dp - one) id)) vsc

\end{haskell}
Generation of array values is worth an explanation: the \HPG\ generates
a pair of values, \prog{v} and \prog{v'}, which are the putative lower
and upper array bounds.
A random number, \prog{r}, up to \prog{max\_array\_len}$-1$ is then generated
and \prog{v''} is set to \prog{v}$+$\prog{r}, if \prog{v'} is
greater than \prog{v}, or \prog{v'} otherwise.
The purpose of this is to prevent enormous arrays from being generated ---
nothing larger than \prog{max\_array\_len} will be produced (the value of
\prog{max\_array\_len} is compiled into the \HPG).
Finally, array values are generated for each index in the (possibly empty)
range \prog{[v..v'']}

\prog{gen\_basic\_val bt c} applies \prog{c} to a value of the basic
type \prog{bt}.
\begin{haskell}

> gen_basic_val :: Base_type -> Vcont -> Cont
> gen_basic_val (Num_type k) vc
>     =  upto maxint (\r -> vc (Num_val k r))
>        where
>        maxint  =  256 :: Int
> gen_basic_val Bool_type vc
>     =  choose [True, False] (\b -> vc (Bool_val b))
> gen_basic_val Char_type vc
>     =  upto maxchar (\r -> vc (Char_val (toEnum r)))

\end{haskell}
The integer generation function is pretty rough and ready.
A production version would have a generator which produced interesting
values rather than completely random ones.
By ``interesting'' we mean values close to powers of two, as these tend
to be the values giving greatest problems for arithmetic implementations
in many languages.

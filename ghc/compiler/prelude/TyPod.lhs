%************************************************************************
%*									*
\section[TyPod]{The Pod datatype}
%*									*
%************************************************************************
\begin{code}
#include "HsVersions.h"

module TyPod where

import PrelFuns		-- help functions, types and things
import TyInteger --ToDo:DPH: no such thing any more!
import TyProcs
import TyBool 		( boolTy )
import Unique

import AbsUniType	( getUniDataTyCon_maybe , mkPodizedPodTyCon )
import Maybes
\end{code}

In the implementation of \DPHaskell{} for a SIMD machine, we adopt three
diffrent models of \POD{}s.

%************************************************************************
\subsection[User]{The Users model}
%************************************************************************
The users model of a \POD{} is outlined in ``Data Parallel Haskell: Mixing old
and new glue''\cite{hill:dpglue}. In this model, a \POD{} represents a
collection of index value pairs, where each index uniquely identifies a
single element of a \POD{}.  As \POD{}s are an abstraction of the processing
elements of a data parallel machine, we choose to collect the index value
pairs into a data type we call a `processor'.

The indices of a \POD{} can be thought of as a subset of the
integers\footnote{10/03/93: I've decided to change the index types of \POD{}'s
---they are now Int's {\em not} Integer's. The use of the GMP package has
changed things, Integers are now special, and there's no way I'm going
to have time to implement them on the DAP. I would like Integers to be like
Ints, i.e a single boxed primitive value --- they are'nt like that any more.
I've therefore plumped for Int's as index values, which means indices
are restricted to 32bit signed values.}. We use
the Haskell class system to extend the range of possible types for the indices
such that any type that is an instance of the class {\tt Pid} (processor
identifier) may be used as an index type.

%************************************************************************
\subsection[prePodized]{The Core Syntax model before podization}
%************************************************************************
Desugaring of the abstract syntax introduces the overloaded operators
{\tt fromDomain} and {\tt toDomain} to convert the index types to integers.
We bring the \POD{} type and processor types closer together in the core
syntax; \POD{}s will have types such as {\tt <<Int,Int;Char>>} in
which the integer types before the ``;'' determine the position of an
element identified by those integers within a two dimensioned \POD{}
(i.e a matrix).
%************************************************************************
\subsection[postPodized]{The Core Syntax model after podization}
%************************************************************************
Things drastically change after podization. There are four different
variety of \POD{}s being used at runtime:
\begin{enumerate}
\item[Interface] A $k$ dimensional Interface \POD{} of $\alpha$'s is
		 represented by a product type that contains a $k$ dimensional
		 inside out \POD{} of Boolean values that determine at what
		 processors the Interface \POD{} is to be defined; and a $k$
		 dimensional inside out \POD{} of $\alpha$'s - the \POD{}s that
		 the user manipulates in \POD{} comprehensions are all
		 interface \POD{}'s --- see note **1** on efficiency below.

\item[Podized]   The remaining types of \POD{}s are invisible to the user
		  - See the podization files for more details (even a bit
		 sketchy their :-(

\item[Primitive] A $k$ dimensional unboxed \POD{} is a contiguous subset of
		 primitive unboxed values - these will hopefully be the
		 staple diet of Data Parallel evaluation. For non SIMD
		 people, these are just like `C' arrays, except we can apply
		 primitive parallel operations to them---for example add
		 two arrays together.

\item[Hard luck] Hard luck \POD{}s are the ones that we cann't implement in a
		 parallel manner - see podization files for more details.
\end{enumerate}

Note **1** : Efficiency of parallel functions.

There are various (trivial) laws concerning \POD{} comprehensions, such as

(vectorMap f) . (vectorMap g) == vectorMap (f.g)

The right of the above expressions is more ``efficient'' because we only
unbox the interface \POD{}, then check for undefined elements once in contrast
to twice in the left expression. Maybe theres some scope here for some
simplifications ??

%************************************************************************
%*									*
\section[User_POD]{The ``Users model'' of a Pod}
%*									*
%************************************************************************
\begin{code}
mkPodTy :: UniType -> UniType
mkPodTy ty = UniData podTyCon [ty]

mkPodNTy:: Int -> UniType -> UniType
mkPodNTy n ty = UniData podTyCon [mkProcessorTy (take n int_tys) ty]
	      where
	         int_tys = integerTy : int_tys

podTyCon = pcDataTyCon podTyConKey pRELUDE_BUILTIN "Pod" [alpha_tv] []
\end{code}

%************************************************************************
%*									*
\section[Podized_POD]{The ``Podized model'' of a Pod}
%*									*
%************************************************************************
Theres a small problem with the following code, I wonder if anyone can help??

I have defined podized versions of TyCons, by wrapping a TyCon and an Int in
a PodizedTyCon (similiar to technique used for Ids). This is helpfull because
when tycons are attached to cases, they show that they are podized (I want
to preserve the info). TyCons are also used in the unitype world, the problem
being if I want a podized dictionary - I cannt just call getUniDataTyCon
to get me the dictionaries TyCon - it doesnt have one :-( What I've therefore
done is get the tycon out of a unitype if it has one, otherwise I use a
default podizedTyConKey which means the things podized, but dont ask anything
about it - (also for polymorphic types).

ToDo(hilly):	Using @getUniDataTyCon_maybe@ doesnt seem a good way of doing
		things...
\begin{code}
mkPodizedPodNTy:: Int -> UniType -> UniType
mkPodizedPodNTy n ty
  = case (getUniDataTyCon_maybe ty) of
     Nothing    ->let tc = pcDataTyCon (podizedPodTyConKey n) pRELUDE_BUILTIN
			               ("PodizedUnk"++show n) [alpha_tv] []
 		  in UniData tc [ty]

     Just (tycon,_,_) ->UniData (mkPodizedPodTyCon n tycon) [ty]

\end{code}
%************************************************************************
%*									*
\section[Podized_POD]{The ``Interface model'' of a Pod}
%*									*
%************************************************************************
\begin{code}
mkInterfacePodNTy n ty
  = UniData (interfacePodTyCon n) [mkPodizedPodNTy n ty]

interfacePodTyCon n
  = pcDataTyCon interfacePodTyConKey pRELUDE_BUILTIN
	        "InterPod" [alpha_tv] [mKINTERPOD_ID n]

mKINTERPOD_ID n
  = pcDataCon interfacePodDataConKey pRELUDE_BUILTIN "MkInterPod"
              [] [] [mkPodizedPodNTy n boolTy] (interfacePodTyCon n) nullSpecEnv
\end{code}

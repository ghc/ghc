%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[TypeRep]{Type - friends' interface}

\begin{code}
module TypeRep (
	Type(..), TyNote(..), 		-- Representation visible 
	SourceType(..), 		-- to friends
	
 	Kind, PredType, ThetaType,		-- Synonyms
	TyVarSubst,

	superKind, superBoxity,				-- KX and BX respectively
	liftedBoxity, unliftedBoxity, 			-- :: BX
	openKindCon, 					-- :: KX
	typeCon,					-- :: BX -> KX
	liftedTypeKind, unliftedTypeKind, openTypeKind,	-- :: KX
	mkArrowKind, mkArrowKinds,			-- :: KX -> KX -> KX

	funTyCon
    ) where

#include "HsVersions.h"

-- friends:
import Var	  ( TyVar )
import VarEnv     ( TyVarEnv )
import VarSet     ( TyVarSet )
import Name	  ( Name )
import BasicTypes ( IPName )
import TyCon	  ( TyCon, KindCon, mkFunTyCon, mkKindCon, mkSuperKindCon )
import Class	  ( Class )
import Binary

-- others
import PrelNames	( superKindName, superBoxityName, liftedConName, 
			  unliftedConName, typeConName, openKindConName, 
			  funTyConName
			)
\end{code}

%************************************************************************
%*									*
\subsection{Type Classifications}
%*									*
%************************************************************************

A type is

	*unboxed*	iff its representation is other than a pointer
			Unboxed types are also unlifted.

	*lifted*	A type is lifted iff it has bottom as an element.
			Closures always have lifted types:  i.e. any
			let-bound identifier in Core must have a lifted
			type.  Operationally, a lifted object is one that
			can be entered.

			Only lifted types may be unified with a type variable.

	*algebraic*	A type with one or more constructors, whether declared
			with "data" or "newtype".   
			An algebraic type is one that can be deconstructed
			with a case expression.  
			*NOT* the same as lifted types,  because we also 
			include unboxed tuples in this classification.

	*data*		A type declared with "data".  Also boxed tuples.

	*primitive*	iff it is a built-in type that can't be expressed
			in Haskell.

Currently, all primitive types are unlifted, but that's not necessarily
the case.  (E.g. Int could be primitive.)

Some primitive types are unboxed, such as Int#, whereas some are boxed
but unlifted (such as ByteArray#).  The only primitive types that we
classify as algebraic are the unboxed tuples.

examples of type classifications:

Type		primitive	boxed		lifted		algebraic    
-----------------------------------------------------------------------------
Int#,		Yes		No		No		No
ByteArray#	Yes		Yes		No		No
(# a, b #)	Yes		No		No		Yes
(  a, b  )	No		Yes		Yes		Yes
[a]		No		Yes		Yes		Yes



	----------------------
	A note about newtypes
	----------------------

Consider
	newtype N = MkN Int

Then we want N to be represented as an Int, and that's what we arrange.
The front end of the compiler [TcType.lhs] treats N as opaque, 
the back end treats it as transparent [Type.lhs].

There's a bit of a problem with recursive newtypes
	newtype P = MkP P
	newtype Q = MkQ (Q->Q)

Here the 'implicit expansion' we get from treating P and Q as transparent
would give rise to infinite types, which in turn makes eqType diverge.
Similarly splitForAllTys and splitFunTys can get into a loop.  

Solution: for recursive newtypes use a coerce, and treat the newtype
and its representation as distinct right through the compiler.  That's
what you get if you use recursive newtypes.  (They are rare, so who
cares if they are a tiny bit less efficient.)

So: non-recursive newtypes are represented using a SourceTy (see below)
    recursive newtypes are represented using a TyConApp

The TyCon still says "I'm a newtype", but we do not represent the
newtype application as a SourceType; instead as a TyConApp.


NOTE: currently [March 02] we regard a newtype as 'recursive' if it's in a
mutually recursive group.  That's a bit conservative: only if there's a loop
consisting only of newtypes do we need consider it as recursive.  But it's
not so easy to discover that, and the situation isn't that common.


%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
type SuperKind = Type
type Kind      = Type

type TyVarSubst = TyVarEnv Type

data Type
  = TyVarTy TyVar

  | AppTy
	Type		-- Function is *not* a TyConApp
	Type

  | TyConApp		-- Application of a TyCon
	TyCon		-- *Invariant* saturated appliations of FunTyCon and
			-- 	synonyms have their own constructors, below.
	[Type]		-- Might not be saturated.

  | FunTy		-- Special case of TyConApp: TyConApp FunTyCon [t1,t2]
	Type
	Type

  | ForAllTy		-- A polymorphic type
	TyVar
	Type	

  | SourceTy		-- A high level source type 
	SourceType	-- ...can be expanded to a representation type...

  | NoteTy 		-- A type with a note attached
	TyNote
	Type		-- The expanded version

data TyNote
  = FTVNote TyVarSet	-- The free type variables of the noted expression

  | SynNote Type	-- Used for type synonyms
			-- The Type is always a TyConApp, and is the un-expanded form.
			-- The type to which the note is attached is the expanded form.

\end{code}

-------------------------------------
 		Source types

A type of the form
	SourceTy sty
represents a value whose type is the Haskell source type sty.
It can be expanded into its representation, but: 

	* The type checker must treat it as opaque
	* The rest of the compiler treats it as transparent

There are two main uses
	a) Haskell predicates
	b) newtypes

Consider these examples:
	f :: (Eq a) => a -> Int
	g :: (?x :: Int -> Int) => a -> Int
	h :: (r\l) => {r} => {l::Int | r}

Here the "Eq a" and "?x :: Int -> Int" and "r\l" are all called *predicates*
Predicates are represented inside GHC by PredType:

\begin{code}
data SourceType 
  = ClassP Class [Type]		-- Class predicate
  | IParam (IPName Name) Type	-- Implicit parameter
  | NType TyCon [Type]		-- A *saturated*, *non-recursive* newtype application
				-- [See notes at top about newtypes]

type PredType  = SourceType	-- A subtype for predicates
type ThetaType = [PredType]
\end{code}

(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using 
	* a FunTy for the double arrow
	* with a PredTy as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (PredTy p) then the predicate p is
represented by evidence (a dictionary, for example, of type (predRepTy p).


%************************************************************************
%*									*
\subsection{Kinds}
%*									*
%************************************************************************

Kinds
~~~~~
kind :: KX = kind -> kind

           | Type liftedness	-- (Type *) is printed as just *
				-- (Type #) is printed as just #

           | OpenKind		-- Can be lifted or unlifted
				-- Printed '?'

           | kv			-- A kind variable; *only* happens during kind checking

boxity :: BX = *	-- Lifted
	     | #	-- Unlifted
	     | bv	-- A boxity variable; *only* happens during kind checking

There's a little subtyping at the kind level:  
	forall b. Type b <: OpenKind

That is, a type of kind (Type b) is OK in a context requiring an OpenKind

OpenKind, written '?', is used as the kind for certain type variables,
in two situations:

1.  The universally quantified type variable(s) for special built-in 
    things like error :: forall (a::?). String -> a. 
    Here, the 'a' can be instantiated to a lifted or unlifted type.  

2.  Kind '?' is also used when the typechecker needs to create a fresh
    type variable, one that may very well later be unified with a type.
    For example, suppose f::a, and we see an application (f x).  Then a
    must be a function type, so we unify a with (b->c).  But what kind
    are b and c?  They can be lifted or unlifted types, or indeed type schemes,
    so we give them kind '?'.

    When the type checker generalises over a bunch of type variables, it
    makes any that still have kind '?' into kind '*'.  So kind '?' is never
    present in an inferred type.


------------------------------------------
Define  KX, the type of a kind
	BX, the type of a boxity

\begin{code}
superKind :: SuperKind 		-- KX, the type of all kinds
superKind = TyConApp (mkSuperKindCon superKindName) []

superBoxity :: SuperKind		-- BX, the type of all boxities
superBoxity = TyConApp (mkSuperKindCon superBoxityName) []
\end{code}

------------------------------------------
Define boxities: @*@ and @#@

\begin{code}
liftedBoxity, unliftedBoxity :: Kind		-- :: BX
liftedBoxity   = TyConApp liftedBoxityCon   []
unliftedBoxity = TyConApp unliftedBoxityCon []

liftedBoxityCon   = mkKindCon liftedConName superBoxity
unliftedBoxityCon = mkKindCon unliftedConName superBoxity
\end{code}

------------------------------------------
Define kinds: Type, Type *, Type #, OpenKind

\begin{code}
typeCon :: KindCon	-- :: BX -> KX
typeCon     = mkKindCon typeConName (superBoxity `FunTy` superKind)

liftedTypeKind, unliftedTypeKind, openTypeKind :: Kind	-- Of superkind superKind

liftedTypeKind   = TyConApp typeCon [liftedBoxity]
unliftedTypeKind = TyConApp typeCon [unliftedBoxity]

openKindCon     = mkKindCon openKindConName superKind
openTypeKind    = TyConApp openKindCon []
\end{code}

------------------------------------------
Define arrow kinds

\begin{code}
mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = k1 `FunTy` k2

mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds
\end{code}

-----------------------------------------------------------------------------
Binary kinds for interface files

\begin{code}
instance Binary Kind where
  put_ bh k@(TyConApp tc [])
	| tc == openKindCon  = putByte bh 0
  put_ bh k@(TyConApp tc [TyConApp bc _])
	| tc == typeCon && bc == liftedBoxityCon   = putByte bh 2
	| tc == typeCon && bc == unliftedBoxityCon = putByte bh 3
  put_ bh (FunTy f a) = do putByte bh 4;	put_ bh f; put_ bh a
  put_ bh _ = error "Binary.put(Kind): strange-looking Kind"

  get bh = do 
	b <- getByte bh
	case b of 
	  0 -> return openTypeKind
	  2 -> return liftedTypeKind
	  3 -> return unliftedTypeKind
	  _ -> do f <- get bh; a <- get bh; return (FunTy f a)
\end{code}

%************************************************************************
%*									*
\subsection{Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
funTyCon = mkFunTyCon funTyConName (mkArrowKinds [liftedTypeKind, liftedTypeKind] liftedTypeKind)
	-- You might think that (->) should have type (? -> ? -> *), and you'd be right
	-- But if we do that we get kind errors when saying
	--	instance Control.Arrow (->)
	-- becuase the expected kind is (*->*->*).  The trouble is that the
	-- expected/actual stuff in the unifier does not go contra-variant, whereas
	-- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
	-- a prefix way, thus:  (->) Int# Int#.  And this is unusual.
\end{code}



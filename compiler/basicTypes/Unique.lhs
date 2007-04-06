%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@
comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).

\begin{code}
module Unique (
	Unique, Uniquable(..), hasKey,

	pprUnique, 

	mkUnique,			-- Used in UniqSupply
	mkUniqueGrimily,		-- Used in UniqSupply only!
	getKey, getKey#,		-- Used in Var, UniqFM, Name only!

	incrUnique,			-- Used for renumbering
	deriveUnique,			-- Ditto
	newTagUnique,			-- Used in CgCase
	initTyVarUnique,

	isTupleKey, 

	-- now all the built-in Uniques (and functions to make them)
	-- [the Oh-So-Wonderful Haskell module system wins again...]
	mkAlphaTyVarUnique,
	mkPrimOpIdUnique,
	mkTupleTyConUnique, mkTupleDataConUnique,
	mkPreludeMiscIdUnique, mkPreludeDataConUnique,
	mkPreludeTyConUnique, mkPreludeClassUnique,
	mkPArrDataConUnique,

	mkBuiltinUnique,
	mkPseudoUniqueC,
	mkPseudoUniqueD,
	mkPseudoUniqueE,
	mkPseudoUniqueH
    ) where

#include "HsVersions.h"

import BasicTypes
import PackageConfig
import FastString
import Outputable
import FastTypes

import GHC.Exts
import Data.Char	( chr, ord )
\end{code}

%************************************************************************
%*									*
\subsection[Unique-type]{@Unique@ type and operations}
%*									*
%************************************************************************

The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:

\begin{code}
data Unique = MkUnique Int#
\end{code}

Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.

\begin{code}
mkUnique	:: Char -> Int -> Unique	-- Builds a unique from pieces
unpkUnique	:: Unique -> (Char, Int)	-- The reverse

mkUniqueGrimily :: Int -> Unique		-- A trap-door for UniqSupply
getKey		:: Unique -> Int		-- for Var
getKey#		:: Unique -> Int#		-- for Var

incrUnique	:: Unique -> Unique
deriveUnique	:: Unique -> Int -> Unique
newTagUnique	:: Unique -> Char -> Unique

isTupleKey	:: Unique -> Bool
\end{code}


\begin{code}
mkUniqueGrimily (I# x) = MkUnique x

{-# INLINE getKey #-}
getKey (MkUnique x) = I# x
{-# INLINE getKey# #-}
getKey# (MkUnique x) = x

incrUnique (MkUnique i) = MkUnique (i +# 1#)

-- deriveUnique uses an 'X' tag so that it won't clash with
-- any of the uniques produced any other way
deriveUnique (MkUnique i) delta = mkUnique 'X' (I# i + delta)

-- newTagUnique changes the "domain" of a unique to a different char
newTagUnique u c = mkUnique c i where (_,i) = unpkUnique u

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x::Int#)

mkUnique (C# c) (I# i)
  = MkUnique (w2i (tag `or#` bits))
  where
    tag  = i2w (ord# c) `uncheckedShiftL#` i2w_s 24#
    bits = i2w i `and#` (i2w 16777215#){-``0x00ffffff''-}

unpkUnique (MkUnique u)
  = let
	tag = C# (chr# (w2i ((i2w u) `uncheckedShiftRL#` (i2w_s 24#))))
	i   = I# (w2i ((i2w u) `and#` (i2w 16777215#){-``0x00ffffff''-}))
    in
    (tag, i)
\end{code}



%************************************************************************
%*									*
\subsection[Uniquable-class]{The @Uniquable@ class}
%*									*
%************************************************************************

\begin{code}
class Uniquable a where
    getUnique :: a -> Unique

hasKey		:: Uniquable a => a -> Unique -> Bool
x `hasKey` k	= getUnique x == k

instance Uniquable FastString where
 getUnique fs = mkUniqueGrimily (I# (uniqueOfFS fs))

instance Uniquable PackageId where
 getUnique pid = getUnique (packageIdFS pid)

instance Uniquable Int where
 getUnique i = mkUniqueGrimily i
\end{code}


%************************************************************************
%*									*
\subsection[Unique-instances]{Instance declarations for @Unique@}
%*									*
%************************************************************************

And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).

\begin{code}
eqUnique (MkUnique u1) (MkUnique u2) = u1 ==# u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <#  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <=# u2

cmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 ==# u2 then EQ else if u1 <# u2 then LT else GT

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
    compare a b = cmpUnique a b

-----------------
instance Uniquable Unique where
    getUnique u = u
\end{code}

We do sometimes make strings with @Uniques@ in them:
\begin{code}
pprUnique :: Unique -> SDoc
pprUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (text (iToBase62 u))

#ifdef UNUSED
pprUnique10 :: Unique -> SDoc
pprUnique10 uniq	-- in base-10, dudes
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (int u)
#endif

finish_ppr 't' u pp_u | u < 26
  =	-- Special case to make v common tyvars, t1, t2, ...
	-- come out as a, b, ... (shorter, easier to read)
    char (chr (ord 'a' + u))
finish_ppr tag u pp_u = char tag <> pp_u

instance Outputable Unique where
    ppr u = pprUnique u

instance Show Unique where
    showsPrec p uniq = showsPrecSDoc p (pprUnique uniq)
\end{code}

%************************************************************************
%*									*
\subsection[Utils-base62]{Base-62 numbers}
%*									*
%************************************************************************

A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.

\begin{code}
iToBase62 :: Int -> String
iToBase62 n@(I# n#) 
  = ASSERT(n >= 0) go n# ""
  where
    go n# cs | n# <# 62# 
	     = case (indexCharOffAddr# chars62# n#) of { c# -> C# c# : cs }
	     | otherwise
	     =	case (quotRem (I# n#) 62)	     of { (I# q#, I# r#) ->
		case (indexCharOffAddr# chars62# r#) of { c#  ->
		go q# (C# c# : cs) }}

    chars62# = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#
\end{code}

%************************************************************************
%*									*
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
%*									*
%************************************************************************

Allocation of unique supply characters:
	v,t,u : for renumbering value-, type- and usage- vars.
	B:   builtin
	C-E: pseudo uniques	(used in native-code generator)
	X:   uniques derived by deriveUnique
	_:   unifiable tyvars   (above)
	0-9: prelude things below

	other a-z: lower case chars for unique supplies.  Used so far:

	d	desugarer
	f	AbsC flattener
	g	SimplStg
	l	ndpFlatten
	n	Native codegen
	r	Hsc name cache
	s	simplifier

\begin{code}
mkAlphaTyVarUnique i            = mkUnique '1' i

mkPreludeClassUnique i		= mkUnique '2' i

-- Prelude type constructors occupy *three* slots.
-- The first is for the tycon itself; the latter two
-- are for the generic to/from Ids.  See TysWiredIn.mk_tc_gen_info.

mkPreludeTyConUnique i		= mkUnique '3' (3*i)
mkTupleTyConUnique Boxed   a	= mkUnique '4' (3*a)
mkTupleTyConUnique Unboxed a	= mkUnique '5' (3*a)

-- Data constructor keys occupy *two* slots.  The first is used for the
-- data constructor itself and its wrapper function (the function that
-- evaluates arguments as necessary and calls the worker). The second is
-- used for the worker function (the function that builds the constructor
-- representation).

mkPreludeDataConUnique i	= mkUnique '6' (2*i)	-- Must be alphabetic
mkTupleDataConUnique Boxed a	= mkUnique '7' (2*a)	-- ditto (*may* be used in C labels)
mkTupleDataConUnique Unboxed a	= mkUnique '8' (2*a)

-- This one is used for a tiresome reason
-- to improve a consistency-checking error check in the renamer
isTupleKey u = case unpkUnique u of
		(tag,_) -> tag == '4' || tag == '5' || tag == '7' || tag == '8'

mkPrimOpIdUnique op		= mkUnique '9' op
mkPreludeMiscIdUnique i		= mkUnique '0' i

-- No numbers left anymore, so I pick something different for the character
-- tag 
mkPArrDataConUnique a	        = mkUnique ':' (2*a)

-- The "tyvar uniques" print specially nicely: a, b, c, etc.
-- See pprUnique for details

initTyVarUnique :: Unique
initTyVarUnique = mkUnique 't' 0

mkPseudoUniqueC, mkPseudoUniqueD, mkPseudoUniqueE, mkPseudoUniqueH,
   mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUniqueC i = mkUnique 'C' i -- used for getUnique on Regs
mkPseudoUniqueD i = mkUnique 'D' i -- used in NCG for getUnique on RealRegs
mkPseudoUniqueE i = mkUnique 'E' i -- used in NCG spiller to create spill VirtualRegs
mkPseudoUniqueH i = mkUnique 'H' i -- used in NCG spiller to create spill VirtualRegs
\end{code}


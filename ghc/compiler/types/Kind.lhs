%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[Kind]{The @Kind@ datatype}

\begin{code}
module Kind (
        GenKind(..),	-- Only visible to friends: TcKind
	Kind,	

	mkArrowKind,
	mkTypeKind,
	mkUnboxedTypeKind,
	mkBoxedTypeKind,

	hasMoreBoxityInfo,
	resultKind, argKind,

	pprKind, pprParendKind,

	isUnboxedTypeKind, isTypeKind, isBoxedTypeKind
    ) where

#include "HsVersions.h"

import Util		( panic, assertPanic )
import Unique		( Unique, pprUnique )
import BasicTypes	( Unused )
import Outputable
\end{code}

\begin{code}
data GenKind flexi
  = TypeKind		-- Any type (incl unboxed types)
  | BoxedTypeKind	-- Any boxed type
  | UnboxedTypeKind	-- Any unboxed type
  | ArrowKind (GenKind flexi) (GenKind flexi)
  | VarKind Unique flexi

type Kind = GenKind Unused	-- No variables at all

instance Eq (GenKind flexi) where
  TypeKind          == TypeKind          = True
  BoxedTypeKind     == BoxedTypeKind	 = True
  UnboxedTypeKind   == UnboxedTypeKind	 = True
  (ArrowKind j1 j2) == (ArrowKind k1 k2) = j1==k1 && j2==k2
  (VarKind u1 _)    == (VarKind u2 _)    = u1==u2
  k1		    == k2		 = False

mkArrowKind 	  = ArrowKind
mkTypeKind  	  = TypeKind
mkUnboxedTypeKind = UnboxedTypeKind
mkBoxedTypeKind   = BoxedTypeKind

isTypeKind :: GenKind flexi -> Bool
isTypeKind TypeKind = True
isTypeKind other    = False

isBoxedTypeKind :: GenKind flexi -> Bool
isBoxedTypeKind BoxedTypeKind = True
isBoxedTypeKind other         = False

isUnboxedTypeKind :: GenKind flexi -> Bool
isUnboxedTypeKind UnboxedTypeKind = True
isUnboxedTypeKind other	 	  = False

hasMoreBoxityInfo :: GenKind flexi -> GenKind flexi -> Bool

BoxedTypeKind 	`hasMoreBoxityInfo` TypeKind	    = True
BoxedTypeKind   `hasMoreBoxityInfo` BoxedTypeKind   = True

UnboxedTypeKind `hasMoreBoxityInfo` TypeKind 	    = True
UnboxedTypeKind `hasMoreBoxityInfo` UnboxedTypeKind = True

TypeKind	`hasMoreBoxityInfo` TypeKind	    = True

kind1@(ArrowKind _ _) `hasMoreBoxityInfo` kind2@(ArrowKind _ _)
  = ASSERT( if kind1 == kind2 then True
	    else pprPanic "hadMoreBoxityInfo" (ppr kind1 <> comma <+> ppr kind2) )
    True
	-- The two kinds can be arrow kinds; for example when unifying
	-- (m1 Int) and (m2 Int) we end up unifying m1 and m2, which should
	-- have the same kind.

kind1		`hasMoreBoxityInfo` kind2	    = False

resultKind :: GenKind flexi -> GenKind flexi	-- Get result from arrow kind
resultKind (ArrowKind _ res_kind) = res_kind
resultKind other_kind 		  = panic "resultKind"

argKind :: GenKind flexi -> GenKind flexi		-- Get argument from arrow kind
argKind (ArrowKind arg_kind _) = arg_kind
argKind other_kind 	       = panic "argKind"
\end{code}

Printing
~~~~~~~~
\begin{code}
instance Outputable (GenKind flexi) where
  ppr kind = pprKind kind

pprKind TypeKind          = text "**"	-- Can be boxed or unboxed
pprKind BoxedTypeKind     = char '*'
pprKind UnboxedTypeKind   = text  "*#"	-- Unboxed
pprKind (ArrowKind k1 k2) = sep [pprParendKind k1, text "->", pprKind k2]
pprKind (VarKind u _)     = char 'k' <> pprUnique u

pprParendKind k@(ArrowKind _ _) = parens (pprKind k)
pprParendKind k		 	= pprKind k
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[Kind]{The @Kind@ datatype}

\begin{code}
#include "HsVersions.h"

module Kind (
	Kind(..),		-- Only visible to friends: TcKind

	mkArrowKind,
	mkTypeKind,
	mkUnboxedTypeKind,
	mkBoxedTypeKind,

	hasMoreBoxityInfo,
	resultKind, argKind,

	pprKind, pprParendKind,

	isUnboxedTypeKind, isTypeKind, isBoxedTypeKind,
	notArrowKind
    ) where

IMP_Ubiq(){-uitous-}

import Util		( panic, assertPanic )

import Outputable	( Outputable(..), pprQuote )
import Pretty
\end{code}

\begin{code}
data Kind
  = TypeKind		-- Any type (incl unboxed types)
  | BoxedTypeKind	-- Any boxed type
  | UnboxedTypeKind	-- Any unboxed type
  | ArrowKind Kind Kind
  deriving Eq

mkArrowKind 	  = ArrowKind
mkTypeKind  	  = TypeKind
mkUnboxedTypeKind = UnboxedTypeKind
mkBoxedTypeKind   = BoxedTypeKind

isTypeKind :: Kind -> Bool
isTypeKind TypeKind = True
isTypeKind other    = False

isBoxedTypeKind :: Kind -> Bool
isBoxedTypeKind BoxedTypeKind = True
isBoxedTypeKind other         = False

isUnboxedTypeKind :: Kind -> Bool
isUnboxedTypeKind UnboxedTypeKind = True
isUnboxedTypeKind other	 	  = False

hasMoreBoxityInfo :: Kind -> Kind -> Bool

BoxedTypeKind 	`hasMoreBoxityInfo` TypeKind	    = True
BoxedTypeKind   `hasMoreBoxityInfo` BoxedTypeKind   = True

UnboxedTypeKind `hasMoreBoxityInfo` TypeKind 	    = True
UnboxedTypeKind `hasMoreBoxityInfo` UnboxedTypeKind = True

TypeKind	`hasMoreBoxityInfo` TypeKind	    = True

kind1@(ArrowKind _ _) `hasMoreBoxityInfo` kind2@(ArrowKind _ _) = ASSERT( kind1 == kind2 )
								  True
	-- The two kinds can be arrow kinds; for example when unifying
	-- (m1 Int) and (m2 Int) we end up unifying m1 and m2, which should
	-- have the same kind.

kind1		`hasMoreBoxityInfo` kind2	    = False

notArrowKind (ArrowKind _ _) = False
notArrowKind other_kind	     = True

resultKind :: Kind -> Kind	-- Get result from arrow kind
resultKind (ArrowKind _ res_kind) = res_kind
resultKind other_kind 		  = panic "resultKind"

argKind :: Kind -> Kind		-- Get argument from arrow kind
argKind (ArrowKind arg_kind _) = arg_kind
argKind other_kind 	       = panic "argKind"
\end{code}

Printing
~~~~~~~~
\begin{code}
instance Outputable Kind where
  ppr sty kind = pprQuote sty $ \ _ -> pprKind kind

pprKind TypeKind        = char '*'	-- Can be boxed or unboxed
pprKind BoxedTypeKind   = char '*'
pprKind UnboxedTypeKind = text  "*#"	-- Unboxed
pprKind (ArrowKind k1 k2) = sep [pprParendKind k1, text "->", pprKind k2]

pprParendKind k@(ArrowKind _ _) = parens (pprKind k)
pprParendKind k		 	= pprKind k
\end{code}

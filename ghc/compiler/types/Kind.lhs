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

	isUnboxedKind, isTypeKind
    ) where

import Ubiq{-uitous-}

import Util		( panic, assertPanic )
--import Outputable	( Outputable(..) )
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

isUnboxedKind :: Kind -> Bool
isUnboxedKind UnboxedTypeKind 	= True
isUnboxedKind other		= False

hasMoreBoxityInfo :: Kind -> Kind -> Bool

BoxedTypeKind 	`hasMoreBoxityInfo` TypeKind	    = True
BoxedTypeKind   `hasMoreBoxityInfo` BoxedTypeKind   = True

UnboxedTypeKind `hasMoreBoxityInfo` TypeKind 	    = True
UnboxedTypeKind `hasMoreBoxityInfo` UnboxedTypeKind = True

TypeKind	`hasMoreBoxityInfo` TypeKind	    = True

kind1	 	`hasMoreBoxityInfo` kind2    	    = ASSERT( notArrowKind kind1 &&
							      notArrowKind kind2 )
						      False

-- Not exported
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
  ppr sty kind = pprKind kind

pprKind TypeKind        = ppStr "*"
pprKind BoxedTypeKind   = ppStr "*b"
pprKind UnboxedTypeKind = ppStr "*u"
pprKind (ArrowKind k1 k2) = ppSep [pprKind_parend k1, ppStr "->", pprKind k2]

pprKind_parend k@(ArrowKind _ _) = ppBesides [ppLparen, pprKind k, ppRparen]
pprKind_parend k		 = pprKind k
\end{code}

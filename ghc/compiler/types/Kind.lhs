%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[Kind]{The @Kind@ datatype}

\begin{code}
module Kind (
	Kind(..),		-- Only visible to friends: TcKind

	mkArrowKind,
	mkTypeKind,
	mkUnboxedTypeKind,
	mkBoxedTypeKind,

	isSubKindOf,
	resultKind, argKind
    ) where

import Ubiq{-uitous-}

import Util		( panic )
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

isSubKindOf :: Kind -> Kind -> Bool

BoxedTypeKind   `isSubKindOf` TypeKind = True
UnboxedTypeKind `isSubKindOf` TypeKind = True
kind1		`isSubKindOf` kind2    = kind1 == kind2

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

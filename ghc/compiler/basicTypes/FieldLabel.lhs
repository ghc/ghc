%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[FieldLabel]{The @FieldLabel@ type}

\begin{code}
#include "HsVersions.h"

module FieldLabel where

IMP_Ubiq(){-uitous-}

import Name		( Name{-instance Eq/Outputable-}, NamedThing(..), nameUnique )
import Type		( SYN_IE(Type) )

import Outputable
import Unique           ( Uniquable(..) )
\end{code}

\begin{code}
data FieldLabel
  = FieldLabel	Name	        -- Also used as the Name of the field selector Id
		Type		-- Type of the field; may have free type variables that
				-- are the tyvar of the constructor
				-- e.g.  data T a = MkT { op1 :: a -> a, op2 :: a -> Int }
				-- The type in the FieldLabel for op1 will be simply (a->a).

		FieldLabelTag	-- Indicates position within constructor
				-- If the same field occurs in more than one constructor
				-- then it'll have a separate FieldLabel on each occasion,
				-- but with a single name (and presumably the same type!)

type FieldLabelTag = Int

mkFieldLabel = FieldLabel

firstFieldLabelTag :: FieldLabelTag
firstFieldLabelTag = 1

allFieldLabelTags :: [FieldLabelTag]
allFieldLabelTags = [1..]

fieldLabelName (FieldLabel n _  _)   = n
fieldLabelType (FieldLabel _ ty _)   = ty
fieldLabelTag  (FieldLabel _ _  tag) = tag

instance Eq FieldLabel where
    (FieldLabel n1 _ _) == (FieldLabel n2 _ _) = n1 == n2

instance Outputable FieldLabel where
    ppr sty (FieldLabel n _ _) = ppr sty n

instance NamedThing FieldLabel where
    getName (FieldLabel n _ _) = n

instance Uniquable FieldLabel where
    uniqueOf (FieldLabel n _ _) = nameUnique n
\end{code}

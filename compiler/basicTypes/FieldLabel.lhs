%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[FieldLabel]{The @FieldLabel@ type}

\begin{code}
module FieldLabel(
	FieldLabel,	-- Abstract

	mkFieldLabel, 
	fieldLabelName,	fieldLabelTyCon, fieldLabelType, fieldLabelTag,

	FieldLabelTag,
	firstFieldLabelTag, allFieldLabelTags
  ) where

#include "HsVersions.h"

import Type( Type )
import TyCon( TyCon )
import Name		( Name{-instance Eq/Outputable-}, NamedThing(..), nameUnique )
import Outputable
import Unique           ( Uniquable(..) )
\end{code}

\begin{code}
data FieldLabel
  = FieldLabel	Name	        -- Also used as the Name of the field selector Id

		TyCon		-- Parent type constructor

		Type		-- Type of the field; may have free type variables that
				-- are the tyvars of its parent *data* constructor, and
				-- those will be the same as the tyvars of its parent *type* constructor
				-- e.g.  data T a = MkT { op1 :: a -> a, op2 :: a -> Int }
				-- The type in the FieldLabel for op1 will be simply (a->a).

		FieldLabelTag	-- Indicates position within constructor
				-- (starting with firstFieldLabelTag)
				--
				-- If the same field occurs in more than one constructor
				-- then it'll have a separate FieldLabel on each occasion,
				-- but with a single name (and presumably the same type!)

type FieldLabelTag = Int

mkFieldLabel = FieldLabel

firstFieldLabelTag :: FieldLabelTag
firstFieldLabelTag = 1

allFieldLabelTags :: [FieldLabelTag]
allFieldLabelTags = [firstFieldLabelTag..]

fieldLabelName  (FieldLabel n _ _  _)   = n
fieldLabelTyCon (FieldLabel _ tc _ _)   = tc
fieldLabelType  (FieldLabel _ _ ty _)   = ty
fieldLabelTag   (FieldLabel _ _ _  tag) = tag

instance Eq FieldLabel where
    fl1 == fl2 = fieldLabelName fl1 == fieldLabelName fl2

instance Outputable FieldLabel where
    ppr fl = ppr (fieldLabelName fl)

instance NamedThing FieldLabel where
    getName = fieldLabelName

instance Uniquable FieldLabel where
    getUnique fl = nameUnique (fieldLabelName fl)
\end{code}

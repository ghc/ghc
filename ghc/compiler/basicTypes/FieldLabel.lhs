%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[FieldLabel]{The @FieldLabel@ type}

\begin{code}
#include "HsVersions.h"

module FieldLabel where

IMP_Ubiq(){-uitous-}

import Name		--( Name{-instance Eq/Outputable-}, nameUnique )
import Type		( SYN_IE(Type) )

import Outputable
import UniqFM           ( SYN_IE(Uniquable) )
\end{code}

\begin{code}
data FieldLabel
  = FieldLabel	Name		-- Also used as the Name of the field selector Id
		Type
		FieldLabelTag

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

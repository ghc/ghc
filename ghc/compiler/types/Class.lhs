%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Class]{The @Class@ datatype}

\begin{code}
module Class (
	Class,

	mkClass,
	classKey, classSelIds, classTyCon,
	classSuperClassTheta,
	classBigSig, classInstEnv,

	ClassInstEnv
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Id	( Id, idType, idName )
import {-# SOURCE #-} TyCon	( TyCon )
import {-# SOURCE #-} Type	( Type )
import {-# SOURCE #-} SpecEnv	( SpecEnv )

import TyCon		( TyCon )
import TyVar		( TyVar )
import Maybes		( assocMaybe )
import Name		( NamedThing(..), Name, getOccName )
import Unique		( Unique, Uniquable(..) )
import BasicTypes	( Unused )
import SrcLoc		( SrcLoc )
import Outputable
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Class-basic]{@Class@: basic definition}
%*									*
%************************************************************************

A @Class@ corresponds to a Greek kappa in the static semantics:

\begin{code}
data Class
  = Class
	Unique		-- Key for fast comparison
	Name

	[TyVar]			-- The class type variables

	[(Class,[Type])]	-- Immediate superclasses, and the
	[Id]			-- corresponding selector functions to
				-- extract them from a dictionary of this
			  	-- class

	[Id]			--	 * selector functions
	[Maybe Id]		--	 * default methods
				-- They are all ordered by tag.  The
				-- selector ids contain unfoldings.

	ClassInstEnv		-- All the instances of this class

	TyCon			-- The data type constructor for dictionaries
				-- of this class

type ClassInstEnv = SpecEnv Id		-- The Ids are dfuns
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> [TyVar]
	-> [(Class,[Type])] -> [Id]
	-> [Id] -> [Maybe Id]
	-> TyCon
	-> ClassInstEnv
	-> Class

mkClass name tyvars super_classes superdict_sels
	dict_sels defms tycon class_insts
  = Class (uniqueOf name) name tyvars
	  super_classes superdict_sels
  	  dict_sels defms
	  class_insts
	  tycon
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
classKey	     (Class key _ _ _ _ _ _ _ _)  = key
classSuperClassTheta (Class _ _ _ scs _ _ _ _ _)  = scs
classSelIds  	     (Class _ _ _ _ _ sels _ _ _) = sels
classTyCon   	     (Class _ _ _ _ _ _ _ _ tc)   = tc
classInstEnv 	     (Class _ _ _ _ _ _ _ env _)  = env

classBigSig (Class _ _ tyvars super_classes sdsels sels defms _ _)
  = (tyvars, super_classes, sdsels, sels, defms)
\end{code}


%************************************************************************
%*									*
\subsection[Class-instances]{Instance declarations for @Class@}
%*									*
%************************************************************************

We compare @Classes@ by their keys (which include @Uniques@).

\begin{code}
instance Eq Class where
    c1 == c2 = classKey c1 == classKey c2
    c1 /= c2 = classKey c1 /= classKey c2

instance Ord Class where
    c1 <= c2 = classKey c1 <= classKey c2
    c1 <  c2 = classKey c1 <  classKey c2
    c1 >= c2 = classKey c1 >= classKey c2
    c1 >  c2 = classKey c1 >  classKey c2
    compare c1 c2 = classKey c1 `compare` classKey c2
\end{code}

\begin{code}
instance Uniquable Class where
    uniqueOf c = classKey c

instance NamedThing Class where
    getName (Class _ n _ _ _ _ _ _ _) = n
\end{code}



%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Class]{The @Class@ datatype}

\begin{code}
module Class (
	Class,

	mkClass,
	classKey, classSelIds, classTyCon,
	classSuperClassTheta,
	classBigSig, classInstEnv
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TyCon	( TyCon )
import {-# SOURCE #-} Type	( Type )
import {-# SOURCE #-} InstEnv	( InstEnv )

import Var		( Id, TyVar )
import Name		( NamedThing(..), Name )
import Unique		( Unique, Uniquable(..) )
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

	InstEnv			-- All the instances of this class

	TyCon			-- The data type constructor for dictionaries
				-- of this class
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> [TyVar]
	-> [(Class,[Type])] -> [Id]
	-> [Id] -> [Maybe Id]
	-> TyCon
	-> InstEnv
	-> Class

mkClass name tyvars super_classes superdict_sels
	dict_sels defms tycon class_insts
  = Class (getUnique name) name tyvars
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
    getUnique c = classKey c

instance NamedThing Class where
    getName (Class _ n _ _ _ _ _ _ _) = n

instance Outputable Class where
    ppr c = ppr (getName c)

instance Show Class where
    showsPrec p c = showsPrecSDoc p (ppr c)
\end{code}



%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Class]{The @Class@ datatype}

\begin{code}
module Class (
	Class, ClassOpItem,

	mkClass, classTyVars,
	classKey, classSelIds, classTyCon,
	classBigSig, classInstEnv
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TyCon	( TyCon )
import {-# SOURCE #-} TypeRep	( Type )
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
  = Class {
	classKey  :: Unique,			-- Key for fast comparison
	className :: Name,
	
	classTyVars :: [TyVar],			-- The class type variables

	classSCTheta :: [(Class,[Type])],	-- Immediate superclasses, and the
	classSCSels  :: [Id],			-- corresponding selector functions to
						-- extract them from a dictionary of this
						-- class

	classOpStuff :: [ClassOpItem],		-- Ordered by tag

	classInstEnv :: InstEnv,	-- All the instances of this class

	classTyCon :: TyCon		-- The data type constructor for dictionaries
  }					-- of this class

type ClassOpItem = (Id, 	--   Selector function; contains unfolding
		    Id, 	--   Default methods
		    Bool)	--   True <=> an explicit default method was 
				--	      supplied in the class decl
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> [TyVar]
	-> [(Class,[Type])] -> [Id]
	-> [(Id, Id, Bool)]
	-> TyCon
	-> InstEnv
	-> Class

mkClass name tyvars super_classes superdict_sels
	op_stuff tycon class_insts
  = Class {	classKey = getUnique name, 
		className = name,
		classTyVars = tyvars,
		classSCTheta = super_classes,
		classSCSels = superdict_sels,
		classOpStuff = op_stuff,
		classInstEnv = class_insts,
		classTyCon = tycon }
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
classSelIds (Class {classSCSels = sc_sels, classOpStuff = op_stuff})
  = sc_sels ++ [op_sel | (op_sel, _, _) <- op_stuff]

classBigSig (Class {classTyVars = tyvars, classSCTheta = sc_theta, 
	 	    classSCSels = sc_sels, classOpStuff = op_stuff})
  = (tyvars, sc_theta, sc_sels, op_stuff)
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
    getName clas = className clas

instance Outputable Class where
    ppr c = ppr (getName c)

instance Show Class where
    showsPrec p c = showsPrecSDoc p (ppr c)
\end{code}



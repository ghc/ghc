%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The @Class@ datatype

\begin{code}
module Class (
	Class, ClassOpItem, 
	DefMeth (..),

	FunDep,	pprFundeps,

	mkClass, classTyVars, classArity,
	classKey, className, classATs, classSelIds, classTyCon, classMethods,
	classBigSig, classExtraBigSig, classTvsFds, classSCTheta
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TyCon	( TyCon )
import {-# SOURCE #-} TypeRep	( PredType )

import Var
import Name
import BasicTypes
import Unique
import Outputable
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
	classKey  :: Unique,		-- Key for fast comparison
	className :: Name,
	
	classTyVars  :: [TyVar],	-- The class type variables
	classFunDeps :: [FunDep TyVar],	-- The functional dependencies

	classSCTheta :: [PredType],	-- Immediate superclasses, and the
	classSCSels  :: [Id],		-- corresponding selector functions
					-- to extract them from a dictionary
					-- of this class

        classATs     :: [TyCon],	-- Associated type families

	classOpStuff :: [ClassOpItem],	-- Ordered by tag

	classTyCon :: TyCon		-- The data type constructor for
					-- dictionaries of this class
     }

type FunDep a = ([a],[a])  --  e.g. class C a b c | a b -> c, a c -> b where...
			   --  Here fun-deps are [([a,b],[c]), ([a,c],[b])]

type ClassOpItem = (Id, DefMeth)
	-- Selector function; contains unfolding
	-- Default-method info

data DefMeth = NoDefMeth 		-- No default method
	     | DefMeth  		-- A polymorphic default method
	     | GenDefMeth 		-- A generic default method
             deriving Eq  
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> [TyVar]
	-> [([TyVar], [TyVar])]
	-> [PredType] -> [Id]
	-> [TyCon]
	-> [ClassOpItem]
	-> TyCon
	-> Class

mkClass name tyvars fds super_classes superdict_sels ats 
	op_stuff tycon
  = Class {	classKey = getUnique name, 
		className = name,
		classTyVars = tyvars,
		classFunDeps = fds,
		classSCTheta = super_classes,
		classSCSels = superdict_sels,
		classATs = ats,
		classOpStuff = op_stuff,
		classTyCon = tycon }
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
classArity :: Class -> Arity
classArity clas = length (classTyVars clas)
	-- Could memoise this

classSelIds :: Class -> [Id]
classSelIds c@(Class {classSCSels = sc_sels})
  = sc_sels ++ classMethods c

classMethods :: Class -> [Id]
classMethods (Class {classOpStuff = op_stuff})
  = [op_sel | (op_sel, _) <- op_stuff]

classTvsFds c
  = (classTyVars c, classFunDeps c)

classBigSig (Class {classTyVars = tyvars, classSCTheta = sc_theta, 
	 	    classSCSels = sc_sels, classOpStuff = op_stuff})
  = (tyvars, sc_theta, sc_sels, op_stuff)
classExtraBigSig (Class {classTyVars = tyvars, classFunDeps = fundeps,
			 classSCTheta = sc_theta, classSCSels = sc_sels,
			 classATs = ats, classOpStuff = op_stuff})
  = (tyvars, fundeps, sc_theta, sc_sels, ats, op_stuff)
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

instance Outputable DefMeth where
    ppr DefMeth     =  text "{- has default method -}"
    ppr GenDefMeth  =  text "{- has generic method -}"
    ppr NoDefMeth   =  empty   -- No default method

pprFundeps :: Outputable a => [FunDep a] -> SDoc
pprFundeps []  = empty
pprFundeps fds = hsep (ptext SLIT("|") : punctuate comma (map ppr_fd fds))
	       where
		 ppr_fd (us, vs) = hsep [interppSP us, ptext SLIT("->"), 
					 interppSP vs]
\end{code}


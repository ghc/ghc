%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The @Class@ datatype

\begin{code}
module Class (
	Class, ClassOpItem, 
	DefMeth (..),
	defMethSpecOfDefMeth,

	FunDep,	pprFundeps, pprFunDep,

	mkClass, classTyVars, classArity, classSCNEqs,
	classKey, className, classATs, classTyCon, classMethods,
	classOpItems, classBigSig, classExtraBigSig, classTvsFds, classSCTheta,
        classAllSelIds, classSCSelId
    ) where

#include "Typeable.h"
#include "HsVersions.h"

import {-# SOURCE #-} TyCon	( TyCon )
import {-# SOURCE #-} TypeRep	( PredType )

import Var
import Name
import BasicTypes
import Unique
import Util
import Outputable
import FastString

import qualified Data.Data as Data
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

	-- Superclasses: eg: (F a ~ b, F b ~ G a, Eq a, Show b)
        -- We need value-level selectors for the dictionary 
	-- superclasses, but not for the equality superclasses
	classSCTheta :: [PredType],	-- Immediate superclasses, 
		     			---   *with equalities first*
        classSCNEqs  :: Int,		-- How many equalities
	classSCSels  :: [Id],		-- Selector functions to extract the
		     			--   *dictionary* superclasses from a 
					--   dictionary of this class
	-- Associated types
        classATs     :: [TyCon],	-- Associated type families

        -- Class operations
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
	     | DefMeth Name  		-- A polymorphic default method
	     | GenDefMeth 		-- A generic default method
             deriving Eq  

-- | Convert a `DefMethSpec` to a `DefMeth`, which discards the name field in
--   the `DefMeth` constructor of the `DefMeth`.
defMethSpecOfDefMeth :: DefMeth -> DefMethSpec
defMethSpecOfDefMeth meth
 = case meth of
	NoDefMeth	-> NoDM
	DefMeth _	-> VanillaDM
	GenDefMeth	-> GenericDM

\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> [TyVar]
	-> [([TyVar], [TyVar])]
	-> [PredType] -> Int -> [Id]
	-> [TyCon]
	-> [ClassOpItem]
	-> TyCon
	-> Class

mkClass name tyvars fds super_classes n_eqs superdict_sels ats 
	op_stuff tycon
  = Class {	classKey     = getUnique name, 
		className    = name,
		classTyVars  = tyvars,
		classFunDeps = fds,
		classSCTheta = super_classes,
                classSCNEqs  = n_eqs,
		classSCSels  = superdict_sels,
		classATs     = ats,
		classOpStuff = op_stuff,
		classTyCon   = tycon }
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

classAllSelIds :: Class -> [Id]
-- Both superclass-dictionary and method selectors
classAllSelIds c@(Class {classSCSels = sc_sels})
  = sc_sels ++ classMethods c

classSCSelId :: Class -> Int -> Id
-- Get the n'th superclass selector Id
-- where n is 0-indexed, and counts 
--    *all* superclasses including equalities
classSCSelId (Class { classSCNEqs = n_eqs, classSCSels = sc_sels }) n
  = ASSERT( sc_sel_index >= 0 && sc_sel_index < length sc_sels )
    sc_sels !! sc_sel_index
  where
    sc_sel_index = n - n_eqs	-- 0-index into classSCSels

classMethods :: Class -> [Id]
classMethods (Class {classOpStuff = op_stuff})
  = [op_sel | (op_sel, _) <- op_stuff]

classOpItems :: Class -> [ClassOpItem]
classOpItems (Class { classOpStuff = op_stuff})
  = op_stuff

classTvsFds :: Class -> ([TyVar], [FunDep TyVar])
classTvsFds c
  = (classTyVars c, classFunDeps c)

classBigSig :: Class -> ([TyVar], [PredType], [Id], [ClassOpItem])
classBigSig (Class {classTyVars = tyvars, classSCTheta = sc_theta, 
	 	    classSCSels = sc_sels, classOpStuff = op_stuff})
  = (tyvars, sc_theta, sc_sels, op_stuff)

classExtraBigSig :: Class -> ([TyVar], [FunDep TyVar], [PredType], [Id], [TyCon], [ClassOpItem])
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
    ppr (DefMeth n) =  ptext (sLit "Default method") <+> ppr n
    ppr GenDefMeth  =  ptext (sLit "Generic default method")
    ppr NoDefMeth   =  empty   -- No default method

pprFundeps :: Outputable a => [FunDep a] -> SDoc
pprFundeps []  = empty
pprFundeps fds = hsep (ptext (sLit "|") : punctuate comma (map pprFunDep fds))

pprFunDep :: Outputable a => FunDep a -> SDoc
pprFunDep (us, vs) = hsep [interppSP us, ptext (sLit "->"), interppSP vs]

instance Data.Typeable Class where
    typeOf _ = Data.mkTyConApp (Data.mkTyCon "Class") []

instance Data.Data Class where
    -- don't traverse?
    toConstr _   = abstractConstr "Class"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "Class"
\end{code}


%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TyVar]{Type variables}

\begin{code}
#include "HsVersions.h"

module TyVar (
	TyVar(..),	-- non-abstract for unifier's benefit
	TyVarTemplate,

	mkUserTyVar, mkPolySysTyVar, mkOpenSysTyVar,
--UNUSED: mkPrimSysTyVar, isPrimTyVar,

--	getTyVarUnique,

	cmpTyVar, eqTyVar, ltTyVar,  -- used a lot!

	mkUserTyVarTemplate, mkSysTyVarTemplate, mkTemplateTyVars, 

	cloneTyVarFromTemplate,
	cloneTyVar,
	instantiateTyVarTemplates,

	-- a supply of template tyvars 
	alphaTyVars,
	alpha_tv, beta_tv, gamma_tv, delta_tv, epsilon_tv,		 -- templates
	alpha_tyvar, beta_tyvar, gamma_tyvar, delta_tyvar, epsilon_tyvar,-- real tyvars

	-- so the module is self-contained...
	ShortName
    ) where

import NameTypes	( ShortName )
import Outputable	-- class for printing, forcing
import Pretty		-- pretty-printing utilities
import SrcLoc		( SrcLoc, mkUnknownSrcLoc )
import Unique
import UniType		( mkTyVarTy, TauType(..), InstTyEnv(..), UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Util

#ifndef __GLASGOW_HASKELL__
{-hide import from mkdependHS-}
import
	Word
#endif
\end{code}

%************************************************************************
%*									*
\subsection[TyVar-basics]{@TyVar@ type and basic operations}
%*									*
%************************************************************************

We distinguish system from user type variables so that the unifier can
bias in terms of replacing system with user ones rather than vice
versa.

\begin{code}
data TyVar
  = PrimSysTyVar  	-- Can only be unified with a primitive type
	Unique	-- Cannot be generalised
			-- Introduced by ccalls
				
  | PolySysTyVar  	-- Can only be unified with a boxed type
	Unique	-- Can be generalised
			-- Introduced when a polymorphic type is instantiated

  | OpenSysTyVar  	-- Can unify with any type at all
	Unique	-- Can be generalised, but remember that the resulting
			-- polymorphic type will be instantiated with PolySysTyVars
			-- Introduced by lambda bindings

  | UserTyVar		-- This is exactly like PolySysTyVar except that it
	Unique	-- has a name attached, derived from something the user typed
	ShortName

-- **** NB: Unboxed but non-primitive things (which don't exist at all at present)
-- 	    are not catered for by the above scheme.

mkPolySysTyVar = PolySysTyVar
mkUserTyVar    = UserTyVar
mkOpenSysTyVar = OpenSysTyVar
--UNUSED:mkPrimSysTyVar = PrimSysTyVar

{-UNUSED
isPrimTyVar (PrimSysTyVar _) = True
isPrimTyVar other	     = False
-}

-- Make a tyvar from a template, given also a unique
cloneTyVarFromTemplate :: TyVarTemplate -> Unique -> TyVar
cloneTyVarFromTemplate (SysTyVarTemplate  _ _) uniq = PolySysTyVar uniq
cloneTyVarFromTemplate (UserTyVarTemplate _ n) uniq = UserTyVar    uniq n

instantiateTyVarTemplates
	::  [TyVarTemplate]
	->  [Unique]
	-> (InstTyEnv,	-- Old-to-new assoc list
	    [TyVar],	-- New type vars
	    [TauType])	-- New type vars wrapped in a UniTyVar
instantiateTyVarTemplates tv_tmpls uniqs
  = --pprTrace "instTyVarTemplates:" (ppr PprDebug new_tys)
    (tv_tmpls `zipEqual` new_tys, new_tyvars, new_tys)
  where
    new_tyvars = zipWith cloneTyVarFromTemplate tv_tmpls uniqs
    new_tys    = map mkTyVarTy new_tyvars

getTyVarUnique :: TyVar -> Unique
getTyVarUnique (PolySysTyVar  u) = u
getTyVarUnique (PrimSysTyVar  u) = u
getTyVarUnique (OpenSysTyVar  u) = u
getTyVarUnique (UserTyVar   u _) = u
\end{code}

Make a new TyVar ``just like'' another one, but w/ a new @Unique@.
Used when cloning big lambdas.  his is only required after
typechecking so the @TyVarUnique@ is just a normal @Unique@.

\begin{code}
cloneTyVar :: TyVar -> Unique -> TyVar

cloneTyVar (PolySysTyVar  _) uniq = PolySysTyVar uniq
cloneTyVar (PrimSysTyVar  _) uniq = PrimSysTyVar uniq
cloneTyVar (OpenSysTyVar  _) uniq = OpenSysTyVar uniq
cloneTyVar (UserTyVar _   n) uniq = UserTyVar    uniq n
\end{code}

%************************************************************************
%*									*
\subsection[TyVar-template]{The @TyVarTemplate@ type}
%*									*
%************************************************************************

A @TyVarTemplate@ is a type variable which is used by @UniForall@ to
universally quantify a type.  It only occurs in a {\em binding}
position in a @UniForall@, not (for example) in a @TyLam@ or
@AbsBinds@.  Every occurrence of a @TyVarTemplate@ in a @UniType@ is
bound by an enclosing @UniForall@, with the sole exception that the
type in a @ClassOp@ has a free @TyVarTemplate@ which is the class type
variable; it is found in the corresponding @Class@ object.

\begin{code}
data TyVarTemplate
  = SysTyVarTemplate  Unique FAST_STRING
  | UserTyVarTemplate Unique ShortName

mkSysTyVarTemplate  = SysTyVarTemplate
mkUserTyVarTemplate = UserTyVarTemplate

getTyVarTemplateUnique (SysTyVarTemplate  u _) = u
getTyVarTemplateUnique (UserTyVarTemplate u _) = u
\end{code}

\begin{code}
alpha_tv, beta_tv, gamma_tv, delta_tv, epsilon_tv :: TyVarTemplate
alpha_tv   = SysTyVarTemplate (mkBuiltinUnique 1) SLIT("a")
beta_tv    = SysTyVarTemplate (mkBuiltinUnique 2) SLIT("b")
gamma_tv   = SysTyVarTemplate (mkBuiltinUnique 3) SLIT("c")
delta_tv   = SysTyVarTemplate (mkBuiltinUnique 4) SLIT("d")
epsilon_tv = SysTyVarTemplate (mkBuiltinUnique 5) SLIT("e")

alpha_tyvar, beta_tyvar, gamma_tyvar, delta_tyvar, epsilon_tyvar :: TyVar
alpha_tyvar   = PolySysTyVar (mkBuiltinUnique 1)
beta_tyvar    = PolySysTyVar (mkBuiltinUnique 2)
gamma_tyvar   = PolySysTyVar (mkBuiltinUnique 3)
delta_tyvar   = PolySysTyVar (mkBuiltinUnique 4)
epsilon_tyvar = PolySysTyVar (mkBuiltinUnique 5)

-- these are used in tuple magic (see TyCon.lhs and Id.lhs)
alphaTyVars :: [TyVarTemplate]
alphaTyVars = alphas_from (10::Int) tyVarStrings
    where
      alphas_from :: Int -> [FAST_STRING] -> [TyVarTemplate]
      alphas_from n (s:ss)
	= SysTyVarTemplate (mkBuiltinUnique n) s : (alphas_from (n+1) ss)

tyVarStrings :: [FAST_STRING]
tyVarStrings
  =  letter_strs {- a..y -} ++ number_strs {- z0 ... zN -}
  where
    letter_strs = [ _PK_ [c]           | c <- ['d' .. 'y'] ]
    number_strs = [ _PK_ ('z': show n) | n <- ([0   .. ] :: [Int]) ]
\end{code}

@mkTemplateTyVars@ creates new template type variables, giving them
the same name and unique as the type variable given to it.  (The name
is for documentation purposes; the unique could just as well be
fresh.)

\begin{code}
mkTemplateTyVars :: [TyVar] -> [TyVarTemplate]

mkTemplateTyVars tyvars
  = zipWith mk_tmpl tyvars tyVarStrings
  where
    mk_tmpl (UserTyVar u name) str = UserTyVarTemplate u name
    mk_tmpl (PolySysTyVar u)   str = SysTyVarTemplate  u str
    mk_tmpl (OpenSysTyVar u)   str = SysTyVarTemplate  u str
\end{code}

%************************************************************************
%*									*
\subsection[TyVar-instances]{Instance declarations for @TyVar@}
%*									*
%************************************************************************

@TyVars@s are compared by comparing their @Unique@s.  (Often!)
\begin{code}
cmpTyVar (PolySysTyVar  u1) (PolySysTyVar  u2) = u1 `cmpUnique` u2
cmpTyVar (PrimSysTyVar  u1) (PrimSysTyVar  u2) = u1 `cmpUnique` u2
cmpTyVar (OpenSysTyVar  u1) (OpenSysTyVar  u2) = u1 `cmpUnique` u2
cmpTyVar (UserTyVar   u1 _) (UserTyVar   u2 _) = u1 `cmpUnique` u2
cmpTyVar other_1	    other_2
  = let tag1 = tag other_1
	tag2 = tag other_2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag (PolySysTyVar  _) = (ILIT(1) :: FAST_INT)
    tag (PrimSysTyVar  _) = ILIT(2)
    tag (OpenSysTyVar  _) = ILIT(3)
    tag (UserTyVar   _ _) = ILIT(4)
\end{code}

\begin{code}
eqTyVar a b = case cmpTyVar a b of { EQ_ -> True;  _   -> False }
ltTyVar a b = case cmpTyVar a b of { LT_ -> True;  EQ_ -> False; GT__ -> False }

instance Eq TyVar where
    a == b = case cmpTyVar a b of { EQ_ -> True;   _ -> False }
    a /= b = case cmpTyVar a b of { EQ_ -> False;  _ -> True  }

instance Ord TyVar where
    a <= b = case cmpTyVar a b of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <  b = case cmpTyVar a b of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case cmpTyVar a b of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >  b = case cmpTyVar a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpTyVar a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}
(@Ord@ for @TyVars@ is needed for the @sortLt@ in @TcSimplify@.)

\begin{code}
instance NamedThing TyVar where
    getExportFlag	tyvar		= NotExported
    isLocallyDefined	tyvar		= True

    getOrigName		(UserTyVar _ n)	= (panic "NamedThing.TyVar.getOrigName(UserTyVar)",
					   getLocalName n)
    getOrigName		tyvar		= (panic "NamedThing.TyVar.getOrigName(SysTyVar)",
					   _PK_ ('t' : (_UNPK_ (showUnique (getTyVarUnique tyvar)))))

    getOccurrenceName	(UserTyVar _ n)	= getOccurrenceName n
    getOccurrenceName	tyvar 		= _PK_ ('t' : (_UNPK_ (showUnique (getTyVarUnique tyvar))))

    getInformingModules  tyvar		= panic "getInformingModule:TyVar"

    getSrcLoc		(UserTyVar _ n)	= getSrcLoc n
    getSrcLoc		_		= mkUnknownSrcLoc

    getTheUnique	tyvar		= getTyVarUnique tyvar

    fromPreludeCore	_		= False
\end{code}

\begin{code}
instance Outputable TyVar where
    ppr sty (PolySysTyVar  u)  = ppr_tyvar sty (ppChar 't')   u
    ppr sty (PrimSysTyVar  u)  = ppr_tyvar sty (ppChar 'p')   u
    ppr sty (OpenSysTyVar  u)  = ppr_tyvar sty (ppChar 'o')   u
    ppr sty (UserTyVar u name) = ppr_tyvar sty (ppr sty name) u

ppr_tyvar sty name u
  = case sty of
      --OLD: PprForUser -> name
      PprDebug       -> pprUnique10 u
      PprUnfolding _ -> pprUnique10 u
      _ 	     -> ppBesides [name, ppChar '.', pprUnique10 u]
\end{code}

%************************************************************************
%*									*
\subsection[TyVarTemplate-instances]{Instance declarations for @TyVarTemplates@}
%*									*
%************************************************************************

\begin{code}
instance Eq TyVarTemplate where
    a == b = getTyVarTemplateUnique a == getTyVarTemplateUnique b
    a /= b = getTyVarTemplateUnique a /= getTyVarTemplateUnique b
\end{code}

\begin{code}
instance Ord TyVarTemplate where
    a <= b = getTyVarTemplateUnique a <= getTyVarTemplateUnique b
    a <  b = getTyVarTemplateUnique a <  getTyVarTemplateUnique b
    a >= b = getTyVarTemplateUnique a >= getTyVarTemplateUnique b
    a >  b = getTyVarTemplateUnique a >  getTyVarTemplateUnique b
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpUnique (getTyVarTemplateUnique a) (getTyVarTemplateUnique b)
		    of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}

\begin{code}
instance NamedThing TyVarTemplate where
    getExportFlag	tyvar		= NotExported
    isLocallyDefined	tyvar		= True

    getOrigName		(UserTyVarTemplate _ n)	= (panic "NamedThing.TyVar.getOrigName(UserTyVarTemplate)",
					   getLocalName n)
    getOrigName		tyvar		= (panic "NamedThing.TyVar.getOrigName(SysTyVarTemplate)",
					   _PK_ ('t' : (_UNPK_ (showUnique (getTyVarTemplateUnique tyvar)))))

    getOccurrenceName	(UserTyVarTemplate _ n)	= getOccurrenceName n
    getOccurrenceName	tyvar 		= _PK_ ('t' : (_UNPK_ (showUnique (getTyVarTemplateUnique tyvar))))

    getInformingModules tyvar		= panic "getInformingModule:TyVarTemplate"

    getSrcLoc		(UserTyVarTemplate _ n)	= getSrcLoc n
    getSrcLoc		_		= mkUnknownSrcLoc

    getTheUnique	tyvar		= getTyVarTemplateUnique tyvar	

    fromPreludeCore	_		= False
\end{code}

\begin{code}
instance Outputable TyVarTemplate where
    ppr sty (SysTyVarTemplate  u name)
      = case sty of
--OLD: 	  PprForUser -> ppPStr name
	  _ 	     -> ppBesides [ppPStr name, ppChar '$', pprUnique10 u]

    ppr sty (UserTyVarTemplate u name)
      = case sty of
--OLD: 	  PprForUser -> ppr sty name
	  _ 	     -> ppBesides [ppr sty name, ppChar '$', pprUnique10 u]
\end{code}

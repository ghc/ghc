%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[UniType]{The UniType data type}

The module @AbsUniType@ is the normal interface to this datatype.
This interface is for ``Friends Only.''

\begin{code}
#include "HsVersions.h"

module UniType (
	UniType(..),	-- not abstract; usually grabbed through AbsUniType

	-- USEFUL SYNONYMS
	SigmaType(..), RhoType(..), TauType(..),
	ThetaType(..),			-- synonym for [(Class,UniType)]
	InstTyEnv(..),

	-- CONSTRUCTION
	mkTyVarTy, mkTyVarTemplateTy, mkDictTy,
	-- use applyTyCon to make UniDatas, UniSyns
	mkRhoTy, mkForallTy, mkSigmaTy,	-- ToDo: perhaps nuke one?

	-- QUANTIFICATION & INSTANTIATION
	quantifyTy,
	instantiateTy,  instantiateTauTy,  instantiateThetaTy,

	-- COMPARISON
	cmpUniType,

	-- PRE-BUILT TYPES (for Prelude)
	alpha, beta, gamma, delta, epsilon,		 	-- these have templates in them
	alpha_ty, beta_ty, gamma_ty, delta_ty, epsilon_ty,	-- these have tyvars in them

	-- to make the interface self-sufficient...
	Class, TyCon, TyVar, TyVarTemplate, Maybe
   ) where

IMPORT_Trace		-- ToDo:rm (debugging only)

#if USE_ATTACK_PRAGMAS
import Class		( cmpClass, getClassSig, Class(..), ClassOp(..) )
#else
import Class		( cmpClass, getClassSig, Class, ClassOp )
#endif
import Maybes		( assocMaybe, Maybe(..) )
import Outputable	-- the output class, etc.
import Pretty
import TyCon		( cmpTyCon, TyCon, Arity(..) )
import TyVar		-- various things
import UniTyFuns	( pprUniType, unDictifyTy
			  IF_ATTACK_PRAGMAS(COMMA pprTyCon)
			)
import Util
\end{code}

%************************************************************************
%*									*
\subsection[UniType-basics]{Basics of the @UniType@ datatype}
%*									*
%************************************************************************

\begin{code}
data UniType
  = 
    -- The free variables of a UniType are always TyVars.
    UniTyVar	TyVar

  | UniFun	UniType	-- Function type
		UniType

  | UniData 		-- Application of a non SynonymTyCon
		TyCon 		-- Must NOT be a SynonymTyCon
		[UniType]	-- Arguments to the type constructor

  | UniSyn		-- Application of a SynonymTyCon
		TyCon 		-- Must be a SynonymTyCon
		[UniType]	-- Arguments to the type constructor
		UniType		-- Expanded version (merely cached here)

  | UniDict	Class
		UniType

  -- The next two are to do with universal quantification

  -- TyVarTemplates only need be unique within a single UniType;
  -- because they are always bound by an enclosing UniForall.
  | UniTyVarTemplate 		
		TyVarTemplate

  | UniForall	TyVarTemplate
		UniType
\end{code}

Universal quantification is over @TyVarTemplate@s.  A type containing
a @UniTyVarTemplate@ always has either an enclosing @UniForall@ which
binds it, or a ``nearby'' binding @TyVarTemplate@.  The only example
of the latter is that a @ClassOp@ will have a free occurrence of the
@TyVarTemplate@ which is held in the @Class@ object.

@UniTyVarTemplate@s are never encountered during unification.

The reasons for this huff and puff over template variables are:
\begin{enumerate}
\item
It's nice to be able to identify them in the code.
\item
It saves worry about accidental capture when instantiating types,
because the types with which the template variables are being
instantiated never themselves contain @UniTyVarTemplates@.
\end{enumerate}

Note: if not @do_properly@, then we treat @UniTyVarTemplates@ as
``wildcards;'' we use this {\em only} when comparing types in STG
land.  It is the responsibility of the caller to strip the
@UniForalls@ off the front.

\begin{code}
cmpUniType do_properly ty1 ty2
  = cmp_ty [] ty1 ty2
  where
    cmp_ty equivs (UniTyVar tv1) (UniTyVar  tv2) = tv1 `cmpTyVar` tv2

    cmp_ty equivs (UniFun a1 b1) (UniFun a2 b2)
      = case cmp_ty equivs a1 a2 of { EQ_ -> cmp_ty equivs b1 b2; other -> other }

    cmp_ty equivs (UniData tc1 tys1) (UniData tc2 tys2)
      = case cmpTyCon tc1 tc2 of { EQ_ -> cmp_ty_lists equivs tys1 tys2; other -> other }

    cmp_ty equivs (UniForall tv1 ty1) (UniForall tv2 ty2)
      = cmp_ty ((tv1,tv2) : equivs) ty1 ty2
\end{code}

Now we deal with the Dict/Dict case.  If the two classes are the same
then all is straightforward.  If not, the two dicts will usually
differ, but (rarely) we could still be looking at two equal
dictionaries!  For example,

     class Foo a => Baz a where

That is, Foo is the only superclass of Baz, and Baz has no methods.
Then a Baz dictionary will be represented simply by a Foo dictionary!

We could sort this out by unDictifying, but that seems like a
sledgehammer to crack a (rather rare) nut.  Instead we ``de-synonym''
each class, by looking to see if it is one of these odd guys which has
no ops and just one superclass (if so, do the same to this
superclass), and then compare the results.

\begin{code}
    cmp_ty equivs (UniDict c1 ty1) (UniDict c2 ty2)
      = case cmpClass c1 c2 of  
	  EQ_   -> cmp_ty equivs ty1 ty2
	  other -> case cmpClass (super_ify c1) (super_ify c2) of
			EQ_   -> cmp_ty equivs ty1 ty2
			other -> other
      where
	super_ify :: Class -> Class	-- Iff the arg is a class with just one
					-- superclass and no operations, then
					-- return super_ify of the superclass,
					-- otherwise just return the original
	super_ify clas
	  = case getClassSig clas of
	      (_, [super_clas], [{-no ops-}]) -> super_ify super_clas
	      other			      -> clas
\end{code}
		
Back to more straightforward things.

\begin{code}
    cmp_ty equivs (UniTyVarTemplate tv1) (UniTyVarTemplate tv2)
      | not do_properly -- STG case: tyvar templates are ``wildcards''
      = EQ_

      | otherwise -- compare properly
      = case (tv1 `cmp_tv_tmpl` tv2) of
	  EQ_ -> EQ_
	  _   -> -- tv1 should Jolly Well be in the equivalents list
		 case assocMaybe equivs tv1 of
		   Just xx -> xx `cmp_tv_tmpl` tv2
		   Nothing ->
#if defined(DEBUG)
			      case (pprPanic "cmpUniType:failed assoc:" (ppCat [ppr PprDebug tv1, ppr PprDebug tv2, ppr PprDebug ty1, ppr PprDebug ty2, ppr PprDebug equivs])) of
#else
			      case (panic "cmpUniType:failed assoc") of
#endif
				s -> -- never get here (BUG)
				     cmp_ty equivs s s

    cmp_ty equivs a@(UniDict _ _) b  = cmp_ty equivs (unDictifyTy a) b
    cmp_ty equivs a b@(UniDict _ _)  = cmp_ty equivs a (unDictifyTy b)

    cmp_ty equivs (UniSyn _ _ expand) b = cmp_ty equivs expand b
    cmp_ty equivs a (UniSyn _ _ expand) = cmp_ty equivs a expand

    -- more special cases for STG case
    cmp_ty equivs (UniTyVarTemplate _) b | not do_properly = EQ_
    cmp_ty equivs a (UniTyVarTemplate _) | not do_properly = EQ_

    cmp_ty equivs other_1 other_2
      = let tag1 = tag other_1
	    tag2 = tag other_2
	in
	if tag1 _LT_ tag2 then LT_ else GT_
      where
	tag (UniTyVar _)	 = (ILIT(1) :: FAST_INT)
	tag (UniFun _ _)	 = ILIT(2)
	tag (UniData _ _)	 = ILIT(3)
	tag (UniDict _ _)	 = ILIT(4)
	tag (UniForall _ _)	 = ILIT(5)
	tag (UniTyVarTemplate _) = ILIT(6)
	tag (UniSyn _ _ _)	 = ILIT(7)

    cmp_tv_tmpl :: TyVarTemplate -> TyVarTemplate -> TAG_
    cmp_tv_tmpl tv1 tv2
      = if tv1 == tv2 then EQ_ else if tv1 < tv2 then LT_ else GT_

    cmp_ty_lists equivs []     []     = EQ_
    cmp_ty_lists equivs (x:xs) []     = GT_
    cmp_ty_lists equivs []     (y:ys) = LT_
    cmp_ty_lists equivs (x:xs) (y:ys)
      = case cmp_ty equivs x y of { EQ_ -> cmp_ty_lists equivs xs ys; other -> other }
\end{code}

\begin{code}
instance Eq  UniType where
    a == b = case cmpUniType True{-properly-} a b of { EQ_ -> True;   _ -> False }
    a /= b = case cmpUniType True{-properly-} a b of { EQ_ -> False;  _ -> True  }
\end{code}

Useful synonyms:

\begin{code}
type SigmaType  = UniType
type RhoType    = UniType		-- No UniForall, UniTyVarTemplate 
type TauType    = UniType		-- No UniDict constructors either
type ThetaType  = [(Class, TauType)]	-- No UniForalls in the UniTypes

type InstTyEnv = [(TyVarTemplate, TauType)]	-- Used for instantiating types
\end{code}

Using @UniType@, a @SigmaType@ such as (Eq a) => a -> [a]
is written as
\begin{verbatim}
UniForall TyVarTemplate
      (UniFun (UniDict Class (UniTyVarTemplate TyVarTemplate))
              (UniFun (UniTyVarTemplate TyVarTemplate)
                      (UniData TyCon [(UniTyVar TyVarTemplate)])))
\end{verbatim}

NB: @mkFunTy@ comes from the prelude.

\begin{code}
mkTyVarTy         = UniTyVar
mkTyVarTemplateTy = UniTyVarTemplate
mkDictTy          = UniDict
-- use applyTyCon to make UniDatas and UniSyns

alpha = UniTyVarTemplate alpha_tv
beta  = UniTyVarTemplate beta_tv
gamma = UniTyVarTemplate gamma_tv
delta = UniTyVarTemplate delta_tv
epsilon = UniTyVarTemplate epsilon_tv

alpha_ty = UniTyVar alpha_tyvar
beta_ty  = UniTyVar beta_tyvar
gamma_ty = UniTyVar gamma_tyvar
delta_ty = UniTyVar delta_tyvar
epsilon_ty = UniTyVar epsilon_tyvar

mkRhoTy :: ThetaType -> TauType -> RhoType
mkRhoTy theta tau
  = foldr mk_dict tau theta
  where
    mk_dict (clas,ty) ty_body = UniFun (UniDict clas ty) ty_body

mkForallTy [] ty = ty
mkForallTy tyvars ty = foldr UniForall ty tyvars

mkSigmaTy :: [TyVarTemplate] -> ThetaType -> TauType -> SigmaType
mkSigmaTy tyvars theta tau = foldr UniForall (mkRhoTy theta tau) tyvars
\end{code}

@quantifyTy@ takes @TyVars@ (not templates) and a  @SigmaType@, and quantifies
over them.  It makes new template type variables, and substitutes for the
original variables in the body.

\begin{code}
quantifyTy :: [TyVar] -> SigmaType -> ([TyVarTemplate], SigmaType)

quantifyTy [] ty = ([], ty)	-- Simple, common case

quantifyTy tyvars ty
 = (templates, foldr UniForall (quant ty) templates)
 where
   templates = mkTemplateTyVars tyvars
   env       = tyvars `zip` (map UniTyVarTemplate templates)

   quant :: SigmaType -> SigmaType	-- Rename the quantified type variables
					-- to their template equivalents

   quant old_ty@(UniTyVar v)  = case (assocMaybe env v) of
				  Nothing -> old_ty	-- We may not be quantifying
							-- over all the type vars!
				  Just ty -> ty

   quant ty@(UniTyVarTemplate v) = ty
   quant ty@(UniData con [])  = ty
   quant (UniData con tys)    = UniData con (map quant tys)
   quant (UniSyn con tys ty)  = UniSyn con (map quant tys) (quant ty)
   quant (UniFun ty1 ty2)     = UniFun (quant ty1) (quant ty2)
   quant (UniDict clas ty)    = UniDict clas (quant ty)

   quant (UniForall tv ty)    =
#ifdef DEBUG
				-- Paranoia check here; shouldn't happen
				if tv `elem` templates then
					panic "quantifyTy"
				else
#endif
					UniForall tv (quant ty)
\end{code}

@instantiateTy@ is the inverse.  It instantiates the free @TyVarTemplates@ 
of a type.  We assume that no inner Foralls bind one of the variables
being instantiated.

\begin{code}
instantiateTy    :: InstTyEnv -> UniType -> UniType

instantiateTy [] ty = ty	-- Simple, common case

instantiateTy env ty 
  = inst ty
  where
    inst ty@(UniTyVar v)      = ty
    inst ty@(UniData con [])  = ty
    inst (UniData con tys)    = UniData con (map inst tys)
    inst (UniFun ty1 ty2)     = UniFun (inst ty1) (inst ty2)
    inst (UniSyn con tys ty)  = UniSyn  con (map inst tys) (inst ty)
    inst (UniDict clas ty)    = UniDict clas (inst ty)
    inst (UniForall v ty)     = UniForall v (inst ty)

    inst old_ty@(UniTyVarTemplate v) = case (assocMaybe env v) of
					 Nothing -> old_ty  -- May partially instantiate
					 Just ty -> ty
\end{code}
The case mentioned in the comment (ie when the template isn't in the envt)
occurs when we instantiate a class op type before instantiating with the class
variable itself.
\begin{code}
instantiateTauTy :: InstTyEnv -> TauType -> TauType
instantiateTauTy tenv ty = instantiateTy tenv ty

instantiateThetaTy :: InstTyEnv -> ThetaType -> ThetaType
instantiateThetaTy tenv theta
 = [(clas,instantiateTauTy tenv ty) | (clas,ty) <- theta]
\end{code}

%************************************************************************
%*									*
\subsection[UniType-instances]{Instance declarations for @UniType@}
%*									*
%************************************************************************

\begin{code}
instance Outputable UniType where
    ppr = pprUniType
\end{code}

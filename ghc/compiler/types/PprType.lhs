%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
#include "HsVersions.h"

module PprType(
	GenTyVar, pprGenTyVar, pprTyVarBndr,
	TyCon, pprTyCon, showTyCon,
	GenType,
	pprGenType, pprParendGenType,
	pprType, pprParendType,
	pprMaybeTy,
	getTypeString,
	specMaybeTysSuffix,
	getTyDescription,
	GenClass, 

	nmbrType, nmbrGlobalType
 ) where

IMP_Ubiq()
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(IdLoop)
#else
import {-# SOURCE #-} Id
#endif


-- friends:
-- (PprType can see all the representations it's trying to print)
import Type		( GenType(..), maybeAppTyCon, Type(..), splitFunTy,
			  splitForAllTy, splitSigmaTy, splitRhoTy, splitAppTys )
import TyVar		( GenTyVar(..), TyVar(..), cloneTyVar )
import TyCon		( TyCon(..), NewOrData )
import Class		( SYN_IE(Class), GenClass(..) )
import Kind		( Kind(..), isBoxedTypeKind, pprParendKind )
import Usage		( pprUVar, GenUsage(..), SYN_IE(Usage), SYN_IE(UVar), cloneUVar )

-- others:
import CStrings		( identToC )
import CmdLineOpts	( opt_OmitInterfacePragmas, opt_PprUserLength )
import Maybes		( maybeToBool )
import Name		(  nameString, Name{-instance Outputable-}, 
			   OccName, pprOccName, getOccString, NamedThing(..)
			)
import Outputable	( PprStyle(..), codeStyle, userStyle, ifaceStyle,
			  ifPprShowAll, interpp'SP, Outputable(..)
			)
import PprEnv
import Pretty
import UniqFM		( UniqFM, addToUFM, emptyUFM, lookupUFM  )
import Unique		( Unique, Uniquable(..), pprUnique10, pprUnique, 
			  incrUnique, listTyConKey, initTyVarUnique 
			)
import Util
\end{code}

\begin{code}
instance (Eq tyvar, Outputable tyvar,
	  Eq uvar,  Outputable uvar  ) => Outputable (GenType tyvar uvar) where
    ppr PprQuote ty = quotes (pprGenType (PprForUser opt_PprUserLength) ty)
    ppr sty ty = pprGenType sty ty

instance Outputable TyCon where
    ppr sty tycon = pprTyCon sty tycon

instance Outputable (GenClass tyvar uvar) where
    -- we use pprIfaceClass for printing in interfaces
    ppr sty (Class u n _ _ _ _ _ _ _) = ppr sty n

instance Outputable (GenTyVar flexi) where
    ppr PprQuote ty = quotes (pprGenTyVar (PprForUser opt_PprUserLength) ty)
    ppr sty tv = pprGenTyVar sty tv

-- and two SPECIALIZEd ones:
instance Outputable {-Type, i.e.:-}(GenType TyVar UVar) where
    ppr PprQuote ty  = quotes (pprGenType (PprForUser opt_PprUserLength) ty)
    ppr other_sty ty = pprGenType other_sty ty

instance Outputable {-TyVar, i.e.:-}(GenTyVar Usage) where
    ppr PprQuote ty   = quotes (pprGenTyVar (PprForUser opt_PprUserLength) ty)
    ppr other_sty  ty = pprGenTyVar other_sty ty
\end{code}

%************************************************************************
%*									*
\subsection[Type]{@Type@}
%*									*
%************************************************************************

Precedence
~~~~~~~~~~
@ppr_ty@ takes an @Int@ that is the precedence of the context.
The precedence levels are:
\begin{description}
\item[tOP_PREC]   No parens required.
\item[fUN_PREC]   Left hand argument of a function arrow.
\item[tYCON_PREC] Argument of a type constructor.
\end{description}


\begin{code}
tOP_PREC    = (0 :: Int)
fUN_PREC    = (1 :: Int)
tYCON_PREC  = (2 :: Int)

maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = parens pretty
\end{code}

@pprGenType@ is the std @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendGenType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendGenType@
works just by setting the initial context precedence very high.

\begin{code}
pprGenType, pprParendGenType :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
		       => PprStyle -> GenType tyvar uvar -> Doc

pprGenType       sty ty = ppr_ty (init_ppr_env sty) tOP_PREC   ty
pprParendGenType sty ty = ppr_ty (init_ppr_env sty) tYCON_PREC ty

pprType, pprParendType :: PprStyle -> Type -> Doc
pprType       	 sty ty = ppr_ty (init_ppr_env_type sty) tOP_PREC   ty
pprParendType 	 sty ty = ppr_ty (init_ppr_env_type sty) tYCON_PREC ty

pprMaybeTy :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
           => PprStyle -> Maybe (GenType tyvar uvar) -> Doc
pprMaybeTy sty Nothing   = char '*'
pprMaybeTy sty (Just ty) = pprParendGenType sty ty
\end{code}

\begin{code}
ppr_ty :: PprEnv tyvar uvar bndr occ -> Int
       -> GenType tyvar uvar
       -> Doc

ppr_ty env ctxt_prec (TyVarTy tyvar)
  = pTyVarO env tyvar

ppr_ty env ctxt_prec (TyConTy tycon usage)
  = ppr_tycon env tycon

ppr_ty env ctxt_prec ty@(ForAllTy _ _)
  | show_forall = maybeParen ctxt_prec fUN_PREC $
		  sep [ ptext SLIT("_forall_"), pp_tyvars, 
			  ppr_theta env theta, ptext SLIT("=>"), pp_body
		        ]
  | null theta = ppr_ty env ctxt_prec body_ty
  | otherwise  = maybeParen ctxt_prec fUN_PREC $
		 sep [ppr_theta env theta, ptext SLIT("=>"), pp_body]
  where
    (tyvars, rho_ty) = splitForAllTy ty
    (theta, body_ty) | show_context = splitRhoTy rho_ty
		     | otherwise    = ([], rho_ty)

    pp_tyvars = brackets (hsep (map (pTyVarB env) tyvars))
    pp_body   = ppr_ty env tOP_PREC body_ty

    sty = pStyle env
    show_forall  = not (userStyle sty)
    show_context = ifaceStyle sty || userStyle sty

ppr_ty env ctxt_prec (ForAllUsageTy uv uvs ty)
  = panic "ppr_ty:ForAllUsageTy"

ppr_ty env ctxt_prec (FunTy ty1 ty2 usage)
    -- We fiddle the precedences passed to left/right branches,
    -- so that right associativity comes out nicely...
  = maybeParen ctxt_prec fUN_PREC (sep (ppr_ty env fUN_PREC ty1 : pp_rest))
  where
    (arg_tys, result_ty) = splitFunTy ty2
    pp_rest = [ ptext SLIT("-> ") <> ppr_ty env fUN_PREC ty | ty <- arg_tys ++ [result_ty] ]

ppr_ty env ctxt_prec ty@(AppTy _ _)
  = ppr_corner env ctxt_prec fun_ty arg_tys
  where
    (fun_ty, arg_tys) = splitAppTys ty

ppr_ty env ctxt_prec (SynTy tycon tys expansion)
  | codeStyle (pStyle env)
	-- always expand types that squeak into C-variable names
  = ppr_ty env ctxt_prec expansion

  | otherwise
  = (<>)
     (ppr_app env ctxt_prec (ppr_tycon env tycon) tys)
     (ifPprShowAll (pStyle env) (hsep [text " {- expansion:",
			       		ppr_ty env tOP_PREC expansion,
				        text "-}"]))

ppr_ty env ctxt_prec (DictTy clas ty usage)
  = braces (ppr_dict env tOP_PREC (clas, ty))
	-- Curlies are temporary


-- Some help functions
ppr_corner env ctxt_prec (TyConTy FunTyCon usage) arg_tys
  | length arg_tys == 2
  = ppr_ty env ctxt_prec (FunTy ty1 ty2 usage)
  where
    (ty1:ty2:_) = arg_tys

ppr_corner env ctxt_prec (TyConTy (TupleTyCon _ _ arity) usage) arg_tys
  |  not (codeStyle (pStyle env))		-- no magic in that case
  && length arg_tys == arity			-- no magic if partially applied
  = parens arg_tys_w_commas
  where
    arg_tys_w_commas = hsep (punctuate comma (map (ppr_ty env tOP_PREC) arg_tys))

ppr_corner env ctxt_prec (TyConTy tycon usage) arg_tys
  | not (codeStyle (pStyle env)) && uniqueOf tycon == listTyConKey
  = ASSERT(length arg_tys == 1)
    brackets (ppr_ty env tOP_PREC ty1)
  where
    (ty1:_) = arg_tys

ppr_corner env ctxt_prec (TyConTy tycon usage) arg_tys
  = ppr_app env ctxt_prec (ppr_tycon env tycon) arg_tys
		      
ppr_corner env ctxt_prec (TyVarTy tyvar) arg_tys
  = ppr_app env ctxt_prec (pTyVarO env tyvar) arg_tys
  

ppr_app env ctxt_prec pp_fun []      
  = pp_fun
ppr_app env ctxt_prec pp_fun arg_tys 
  = maybeParen ctxt_prec tYCON_PREC (hsep [pp_fun, arg_tys_w_spaces])
  where
    arg_tys_w_spaces = hsep (map (ppr_ty env tYCON_PREC) arg_tys)


ppr_theta env []    = empty
ppr_theta env theta = braces (hsep (punctuate comma (map (ppr_dict env tOP_PREC) theta)))

ppr_dict env ctxt_prec (clas, ty)
  = maybeParen ctxt_prec tYCON_PREC
	(hsep [ppr_class env clas, ppr_ty env tYCON_PREC ty]) 
\end{code}

\begin{code}
	-- This one uses only "ppr"
init_ppr_env sty
  = initPprEnv sty b b b b (Just (ppr sty)) (Just (ppr sty)) (Just (ppr sty)) b b b b b
  where
    b = panic "PprType:init_ppr_env"

	-- This one uses pprTyVarBndr, and thus is specific to GenTyVar's types
init_ppr_env_type sty
  = initPprEnv sty b b b b (Just (pprTyVarBndr sty)) (Just (ppr sty)) (Just (ppr sty)) b b b b b
  where
    b = panic "PprType:init_ppr_env"

ppr_tycon  env tycon = ppr (pStyle env) tycon
ppr_class  env clas  = ppr (pStyle env) clas
\end{code}

%************************************************************************
%*									*
\subsection[TyVar]{@TyVar@}
%*									*
%************************************************************************

\begin{code}
pprGenTyVar sty (TyVar uniq kind maybe_name usage)
  = case maybe_name of
      	-- If the tyvar has a name we can safely use just it, I think
	Just n  -> pprOccName sty (getOccName n) <> debug_extra
	Nothing -> pp_kind <> pprUnique uniq
  where
    pp_kind = case kind of
		TypeKind        -> char 'o'
		BoxedTypeKind   -> char 't'
		UnboxedTypeKind -> char 'u'
		ArrowKind _ _   -> char 'a'

    debug_extra = case sty of
		     PprDebug   -> pp_debug
		     PprShowAll -> pp_debug
		     other      -> empty

    pp_debug = text "_" <> pp_kind <> pprUnique uniq
\end{code}

We print type-variable binders with their kinds in interface files.

\begin{code}
pprTyVarBndr sty@PprInterface tyvar@(TyVar uniq kind name usage)
  | not (isBoxedTypeKind kind)
  = hcat [pprGenTyVar sty tyvar, text " :: ", pprParendKind kind]
	-- See comments with ppDcolon in PprCore.lhs

pprTyVarBndr sty tyvar = pprGenTyVar sty tyvar
\end{code}

%************************************************************************
%*									*
\subsection[TyCon]{@TyCon@}
%*									*
%************************************************************************

ToDo; all this is suspiciously like getOccName!

\begin{code}
showTyCon :: PprStyle -> TyCon -> String
showTyCon sty tycon = show (pprTyCon sty tycon)

pprTyCon :: PprStyle -> TyCon -> Doc
pprTyCon sty tycon = ppr sty (getName tycon)
\end{code}



%************************************************************************
%*									*
\subsection{Mumbo jumbo}
%*									*
%************************************************************************

\begin{code}
    -- Shallowly magical; converts a type into something
    -- vaguely close to what can be used in C identifier.
    -- Produces things like what we have in mkCompoundName,
    -- which can be "dot"ted together...

getTypeString :: Type -> FAST_STRING

getTypeString ty
  = case (splitAppTys ty) of { (tc, args) ->
    _CONCAT_ (do_tc tc : map do_arg_ty args) }
  where
    do_tc (TyConTy tc _) = nameString (getName tc)
    do_tc (SynTy _ _ ty) = do_tc ty
    do_tc other = --pprTrace "getTypeString:do_tc:" (pprType PprDebug other) $
		  (_PK_ (show (pprType PprForC other)))

    do_arg_ty (TyConTy tc _) = nameString (getName tc)
    do_arg_ty (TyVarTy tv)   = _PK_ (show (ppr PprForC tv))
    do_arg_ty (SynTy _ _ ty) = do_arg_ty ty
    do_arg_ty other	     = --pprTrace "getTypeString:do_arg_ty:" (pprType PprDebug other) $
			       _PK_ (show (pprType PprForC other))

	-- PprForC expands type synonyms as it goes;
	-- it also forces consistent naming of tycons
	-- (e.g., can't have both "(,) a b" and "(a,b)":
	-- must be consistent!

specMaybeTysSuffix :: [Maybe Type] -> FAST_STRING
specMaybeTysSuffix ty_maybes
  = panic "PprType.specMaybeTysSuffix"
{- LATER:
  = let
	ty_strs  = concat (map typeMaybeString ty_maybes)
	dotted_tys = [ _CONS_ '.' str | str <- ty_strs ]
    in
    _CONCAT_ dotted_tys
-}
\end{code}

Grab a name for the type. This is used to determine the type
description for profiling.
\begin{code}
getTyDescription :: Type -> String

getTyDescription ty
  = case (splitSigmaTy ty) of { (_, _, tau_ty) ->
    case tau_ty of
      TyVarTy _	      -> "*"
      AppTy fun _     -> getTyDescription fun
      FunTy _ res _   -> '-' : '>' : fun_result res
      TyConTy tycon _ -> getOccString tycon
      SynTy tycon _ _ -> getOccString tycon
      DictTy _ _ _    -> "dict"
      ForAllTy _ ty   -> getTyDescription ty
      _		      -> pprPanic "getTyDescription: other" (pprType PprDebug tau_ty)
    }
  where
    fun_result (FunTy _ res _) = '>' : fun_result res
    fun_result other	       = getTyDescription other
\end{code}



%************************************************************************
%*									*
\subsection{Renumbering types}
%*									*
%************************************************************************

We tend to {\em renumber} everything before printing, so that we get
consistent Uniques on everything from run to run.


\begin{code}
nmbrGlobalType :: Type -> Type		-- Renumber a top-level type
nmbrGlobalType ty = nmbrType (\tyvar -> tyvar) (\uvar -> uvar) initTyVarUnique ty

nmbrType :: (TyVar -> TyVar) -> (UVar  -> UVar)		-- Mapping for free vars
	 -> Unique
	 -> Type
	 -> Type

nmbrType tyvar_env uvar_env uniq ty
  = initNmbr tyvar_env uvar_env uniq (nmbrTy ty)

nmbrTy :: Type -> NmbrM Type

nmbrTy (TyVarTy tv)
  = lookupTyVar tv    `thenNmbr` \ new_tv ->
    returnNmbr (TyVarTy new_tv)

nmbrTy (AppTy t1 t2)
  = nmbrTy t1	    `thenNmbr` \ new_t1 ->
    nmbrTy t2	    `thenNmbr` \ new_t2 ->
    returnNmbr (AppTy new_t1 new_t2)

nmbrTy (TyConTy tc use)
  = nmbrUsage use   `thenNmbr` \ new_use ->
    returnNmbr (TyConTy tc new_use)

nmbrTy (SynTy tc args expand)
  = mapNmbr nmbrTy args   `thenNmbr` \ new_args ->
    nmbrTy expand	    `thenNmbr` \ new_expand ->
    returnNmbr (SynTy tc new_args new_expand)

nmbrTy (ForAllTy tv ty)
  = addTyVar tv		$ \ new_tv ->
    nmbrTy ty		`thenNmbr` \ new_ty ->
    returnNmbr (ForAllTy new_tv new_ty)

nmbrTy (ForAllUsageTy u us ty)
  = addUVar u			$ \ new_u  ->
    mapNmbr lookupUVar us	`thenNmbr` \ new_us ->
    nmbrTy ty			`thenNmbr` \ new_ty ->
    returnNmbr (ForAllUsageTy new_u new_us new_ty)

nmbrTy (FunTy t1 t2 use)
  = nmbrTy t1	    `thenNmbr` \ new_t1 ->
    nmbrTy t2	    `thenNmbr` \ new_t2 ->
    nmbrUsage use   `thenNmbr` \ new_use ->
    returnNmbr (FunTy new_t1 new_t2 new_use)

nmbrTy (DictTy c ty use)
  = nmbrTy  ty    `thenNmbr` \ new_ty  ->
    nmbrUsage use   `thenNmbr` \ new_use ->
    returnNmbr (DictTy c new_ty new_use)



lookupTyVar tyvar (NmbrEnv tv_fn tv_env _ _) uniq
  = (uniq, tyvar')
  where
    tyvar' = case lookupUFM tv_env tyvar of
		Just tyvar' -> tyvar'
		Nothing     -> tv_fn tyvar

addTyVar tv m (NmbrEnv f_tv tv_ufm f_uv uv_ufm) u
  = m tv' nenv u'
  where
    nenv    = NmbrEnv f_tv tv_ufm' f_uv uv_ufm
    tv_ufm' = addToUFM tv_ufm tv tv'
    tv'	    = cloneTyVar tv u
    u'      = incrUnique u
\end{code}

Usage stuff

\begin{code}
nmbrUsage (UsageVar v)
  = lookupUVar v	`thenNmbr` \ v' ->
    returnNmbr (UsageVar v)

nmbrUsage u = returnNmbr u


lookupUVar uvar (NmbrEnv _ _ uv_fn uv_env) uniq
  = (uniq, uvar')
  where
    uvar' = case lookupUFM uv_env uvar of
		Just uvar' -> uvar'
		Nothing     -> uv_fn uvar

addUVar uv m (NmbrEnv f_tv tv_ufm f_uv uv_ufm) u
  = m uv' nenv u'
  where
    nenv    = NmbrEnv f_tv tv_ufm f_uv uv_ufm'
    uv_ufm' = addToUFM uv_ufm uv uv'
    uv'	    = cloneUVar uv u
    u'      = incrUnique u
\end{code}

Monad stuff

\begin{code}
data NmbrEnv
  = NmbrEnv	(TyVar -> TyVar) (UniqFM TyVar)		-- Global and local map for tyvars
		(UVar  -> UVar)  (UniqFM UVar)		-- ... for usage vars

type NmbrM a = NmbrEnv -> Unique -> (Unique, a)		-- Unique is name supply

initNmbr :: (TyVar -> TyVar) -> (UVar -> UVar) -> Unique -> NmbrM a -> a
initNmbr tyvar_env uvar_env uniq m
  = let
	init_nmbr_env  = NmbrEnv tyvar_env emptyUFM uvar_env emptyUFM
    in
    snd (m init_nmbr_env uniq)

returnNmbr x nenv u = (u, x)

thenNmbr m k nenv u
  = let
	(u', res) = m nenv u
    in
    k res nenv u'


mapNmbr f []     = returnNmbr []
mapNmbr f (x:xs)
  = f x		    `thenNmbr` \ r  ->
    mapNmbr f xs    `thenNmbr` \ rs ->
    returnNmbr (r:rs)
\end{code}

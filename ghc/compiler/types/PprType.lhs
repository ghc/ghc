%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprType]{Printing Types, TyVars, Classes, ClassOps, TyCons}

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
	GenClassOp, pprGenClassOp,

	addTyVar{-ToDo:don't export-}, nmbrTyVar,
	addUVar,  nmbrUsage,
	nmbrType, nmbrTyCon, nmbrClass
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
import TyVar		( GenTyVar(..), TyVar(..) )
import TyCon		( TyCon(..), NewOrData )
import Class		( SYN_IE(Class), GenClass(..),
			  SYN_IE(ClassOp), GenClassOp(..) )
import Kind		( Kind(..), isBoxedTypeKind, pprParendKind )
import Usage		( pprUVar, GenUsage(..), SYN_IE(Usage), SYN_IE(UVar) )

-- others:
import CStrings		( identToC )
import CmdLineOpts	( opt_OmitInterfacePragmas, opt_PprUserLength )
import Maybes		( maybeToBool )
import Name	{-	(  nameString, Name{-instance Outputable-}, 
			   OccName, pprOccName, getOccString
			) -}
import Outputable	( PprStyle(..), codeStyle, userStyle, ifaceStyle,
			  ifPprShowAll, interpp'SP, Outputable(..) )
import PprEnv
import Pretty
import UniqFM		( addToUFM_Directly, lookupUFM_Directly )
import Unique		( Uniquable(..), pprUnique10, pprUnique, incrUnique, listTyConKey )
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
    ppr sty (Class u n _ _ _ _ _ _ _ _) = ppr sty n

instance Outputable ty => Outputable (GenClassOp ty) where
    ppr sty clsop = pprGenClassOp sty clsop

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
pprGenTyVar sty (TyVar uniq kind name usage)
  | codeStyle sty
  = pp_u
  | otherwise
  = case sty of
      PprInterface -> pp_u
      _		   -> hcat [pp_name, text "{-", pp_u, text "-}"]
   where
    pp_u    = pprUnique uniq
    pp_name = case name of
		Just n  -> pprOccName sty (getOccName n)
		Nothing -> case kind of
				TypeKind        -> char 'o'
				BoxedTypeKind   -> char 't'
				UnboxedTypeKind -> char 'u'
				ArrowKind _ _   -> char 'a'
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
\subsection[Class]{@Class@}
%*									*
%************************************************************************

\begin{code}
pprGenClassOp :: Outputable ty => PprStyle -> GenClassOp ty -> Doc

pprGenClassOp sty op = ppr_class_op sty [] op

ppr_class_op sty tyvars (ClassOp op_name i ty)
  = case sty of
      PprInterface  -> pp_sigd
      PprShowAll    -> pp_sigd
      _		    -> pp_other
  where
    pp_other = ppr sty op_name
    pp_sigd = hsep [pp_other, ptext SLIT("::"), ppr sty ty]
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

ToDo: possibly move:
\begin{code}
nmbrType :: Type -> NmbrM Type

nmbrType (TyVarTy tv)
  = nmbrTyVar tv    `thenNmbr` \ new_tv ->
    returnNmbr (TyVarTy new_tv)

nmbrType (AppTy t1 t2)
  = nmbrType t1	    `thenNmbr` \ new_t1 ->
    nmbrType t2	    `thenNmbr` \ new_t2 ->
    returnNmbr (AppTy new_t1 new_t2)

nmbrType (TyConTy tc use)
  = --nmbrTyCon tc    `thenNmbr` \ new_tc ->
    nmbrUsage use   `thenNmbr` \ new_use ->
    returnNmbr (TyConTy tc new_use)

nmbrType (SynTy tc args expand)
  = --nmbrTyCon tc	    `thenNmbr` \ new_tc ->
    mapNmbr nmbrType args   `thenNmbr` \ new_args ->
    nmbrType expand	    `thenNmbr` \ new_expand ->
    returnNmbr (SynTy tc new_args new_expand)

nmbrType (ForAllTy tv ty)
  = addTyVar tv		`thenNmbr` \ new_tv ->
    nmbrType ty		`thenNmbr` \ new_ty ->
    returnNmbr (ForAllTy new_tv new_ty)

nmbrType (ForAllUsageTy u us ty)
  = addUVar u		    `thenNmbr` \ new_u  ->
    mapNmbr nmbrUVar us     `thenNmbr` \ new_us ->
    nmbrType ty		    `thenNmbr` \ new_ty ->
    returnNmbr (ForAllUsageTy new_u new_us new_ty)

nmbrType (FunTy t1 t2 use)
  = nmbrType t1	    `thenNmbr` \ new_t1 ->
    nmbrType t2	    `thenNmbr` \ new_t2 ->
    nmbrUsage use   `thenNmbr` \ new_use ->
    returnNmbr (FunTy new_t1 new_t2 new_use)

nmbrType (DictTy c ty use)
  = --nmbrClass c	    `thenNmbr` \ new_c   ->
    nmbrType  ty    `thenNmbr` \ new_ty  ->
    nmbrUsage use   `thenNmbr` \ new_use ->
    returnNmbr (DictTy c new_ty new_use)
\end{code}

\begin{code}
addTyVar, nmbrTyVar :: TyVar -> NmbrM TyVar

addTyVar tv@(TyVar u k maybe_name use) nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = --pprTrace "addTyVar:" (hsep [pprUnique u, pprUnique ut]) $
    case (lookupUFM_Directly tvenv u) of
      Just xx -> -- pprTrace "addTyVar: already in map!" (ppr PprDebug tv) $
		 -- (It gets triggered when we do a datatype: first we
		 -- "addTyVar" the tyvars for the datatype as a whole;
		 -- we will subsequently "addId" the data cons, including
		 -- the type for each of them -- each of which includes
		 -- _forall_ ...tvs..., which we will addTyVar.
		 -- Harmless, if that's all that happens....
		 (nenv, xx)
      Nothing ->
	let
	    nenv_plus_tv     = NmbrEnv ui (incrUnique ut) uu
				       idenv
				       (addToUFM_Directly tvenv u new_tv)
				       uvenv

	    (nenv2, new_use) = nmbrUsage use nenv_plus_tv

	    new_tv = TyVar ut k maybe_name new_use
	in
	(nenv2, new_tv)

nmbrTyVar tv@(TyVar u _ _ _) nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = case (lookupUFM_Directly tvenv u) of
      Just xx -> (nenv, xx)
      Nothing ->
	--pprTrace "nmbrTyVar: lookup failed:" (hsep (ppr PprDebug u : [hsep [ppr PprDebug x, ptext SLIT("=>"), ppr PprDebug tv] | (x,tv) <- ufmToList tvenv])) $
	(nenv, tv)
\end{code}

nmbrTyCon : only called from ``top-level'', if you know what I mean.
\begin{code}
nmbrTyCon tc@FunTyCon		  = returnNmbr tc
nmbrTyCon tc@(TupleTyCon _ _ _)	  = returnNmbr tc
nmbrTyCon tc@(PrimTyCon  _ _ _ _) = returnNmbr tc

nmbrTyCon (DataTyCon u n k tvs theta cons clss nod)
  = --pprTrace "nmbrDataTyCon:" (hsep (map (ppr PprDebug) tvs)) $
    mapNmbr addTyVar   tvs	`thenNmbr` \ new_tvs   ->
    mapNmbr nmbr_theta theta	`thenNmbr` \ new_theta ->
    mapNmbr nmbrId     cons	`thenNmbr` \ new_cons  ->
    returnNmbr (DataTyCon u n k new_tvs new_theta new_cons clss nod)
  where
    nmbr_theta (c,t)
      = --nmbrClass c	`thenNmbr` \ new_c ->
        nmbrType  t	`thenNmbr` \ new_t ->
	returnNmbr (c, new_t)

nmbrTyCon (SynTyCon u n k a tvs expand)
  = mapNmbr addTyVar   tvs	`thenNmbr` \ new_tvs ->
    nmbrType	       expand	`thenNmbr` \ new_expand ->
    returnNmbr (SynTyCon u n k a new_tvs new_expand)

nmbrTyCon (SpecTyCon tc specs)
  = mapNmbr nmbrMaybeTy specs	`thenNmbr` \ new_specs ->
    returnNmbr (SpecTyCon tc new_specs)

-----------
nmbrMaybeTy Nothing  = returnNmbr Nothing
nmbrMaybeTy (Just t) = nmbrType t `thenNmbr` \ new_t ->
		       returnNmbr (Just new_t)
\end{code}

\begin{code}
nmbrClass (Class u n tv supers ssels ops osels odefms instenv isupers)
  = addTyVar tv		`thenNmbr` \ new_tv  ->
    mapNmbr nmbr_op ops	`thenNmbr` \ new_ops ->
    returnNmbr (Class u n new_tv supers ssels new_ops osels odefms instenv isupers)
  where
    nmbr_op (ClassOp n tag ty)
      = nmbrType ty	`thenNmbr` \ new_ty ->
	returnNmbr (ClassOp n tag new_ty)
\end{code}

\begin{code}
nmbrUsage :: Usage -> NmbrM Usage

nmbrUsage u = returnNmbr u
{- LATER:
nmbrUsage u@UsageOne   = returnNmbr u
nmbrUsage u@UsageOmega = returnNmbr u
nmbrUsage (UsageVar u)
  = nmbrUVar u	`thenNmbr` \ new_u ->
    returnNmbr (UsageVar new_u)
-}
\end{code}

\begin{code}
addUVar, nmbrUVar :: UVar -> NmbrM UVar

addUVar u nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = case (lookupUFM_Directly uvenv u) of
      Just xx -> trace "addUVar: already in map!" $
		 (nenv, xx)
      Nothing ->
	let
	    nenv_plus_uv     = NmbrEnv ui ut (incrUnique uu)
				       idenv
				       tvenv
				       (addToUFM_Directly uvenv u new_uv)
	    new_uv = uu
	in
	(nenv_plus_uv, new_uv)

nmbrUVar u nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = case (lookupUFM_Directly uvenv u) of
      Just xx -> (nenv, xx)
      Nothing ->
	trace "nmbrUVar: lookup failed" $
	(nenv, u)
\end{code}

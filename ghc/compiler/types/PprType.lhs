%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprType]{Printing Types, TyVars, Classes, ClassOps, TyCons}

\begin{code}
#include "HsVersions.h"

module PprType(
	GenTyVar, pprGenTyVar,
	TyCon, pprTyCon, showTyCon,
	GenType,
	pprGenType, pprParendGenType,
	pprType, pprParendType,
	pprMaybeTy,
	getTypeString,
	typeMaybeString,
	specMaybeTysSuffix,
	getTyDescription,
	GenClass, 
	GenClassOp, pprGenClassOp,
	
	addTyVar{-ToDo:don't export-}, nmbrTyVar,
	addUVar,  nmbrUsage,
	nmbrType, nmbrTyCon, nmbrClass
 ) where

IMP_Ubiq()
IMPORT_DELOOPER(IdLoop)
--IMPORT_DELOOPER(TyLoop) 	-- for paranoia checking

-- friends:
-- (PprType can see all the representations it's trying to print)
import Type		( GenType(..), maybeAppTyCon,
			  splitForAllTy, splitSigmaTy, splitRhoTy, splitAppTy )
import TyVar		( GenTyVar(..) )
import TyCon		( TyCon(..), NewOrData )
import Class		( SYN_IE(Class), GenClass(..),
			  SYN_IE(ClassOp), GenClassOp(..) )
import Kind		( Kind(..) )
import Usage		( pprUVar, GenUsage(..), SYN_IE(Usage), SYN_IE(UVar) )

-- others:
import CStrings		( identToC )
import CmdLineOpts	( opt_OmitInterfacePragmas )
import Maybes		( maybeToBool )
import Name		( isLexVarSym, isLexSpecialSym, origName, moduleOf,
			  getLocalName, Name{-instance Outputable-}
			)
import Outputable	( ifPprShowAll, interpp'SP )
import PprEnv
import PprStyle		( PprStyle(..), codeStyle, showUserishTypes )
import Pretty
import UniqFM		( addToUFM_Directly, lookupUFM_Directly{-, ufmToList ToDo:rm-} )
import Unique		( pprUnique10, pprUnique, incrUnique, listTyConKey )
import Util
\end{code}

\begin{code}
instance (Eq tyvar, Outputable tyvar,
	  Eq uvar,  Outputable uvar  ) => Outputable (GenType tyvar uvar) where
    ppr sty ty = pprGenType sty ty

instance Outputable TyCon where
    ppr sty tycon = pprTyCon sty tycon

instance Outputable (GenClass tyvar uvar) where
    -- we use pprIfaceClass for printing in interfaces
    ppr sty (Class u n _ _ _ _ _ _ _ _) = ppr sty n

instance Outputable ty => Outputable (GenClassOp ty) where
    ppr sty clsop = pprGenClassOp sty clsop

instance Outputable (GenTyVar flexi) where
    ppr sty tv = pprGenTyVar sty tv

-- and two SPECIALIZEd ones:
instance Outputable {-Type, i.e.:-}(GenType TyVar UVar) where
    ppr sty ty = pprGenType sty ty

instance Outputable {-TyVar, i.e.:-}(GenTyVar Usage) where
    ppr sty ty = pprGenTyVar sty ty
\end{code}

%************************************************************************
%*									*
\subsection[Type]{@Type@}
%*									*
%************************************************************************

@pprGenType@ is the std @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendGenType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendGenType@
works just by setting the initial context precedence very high.

\begin{code}
pprGenType, pprParendGenType :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
		       => PprStyle -> GenType tyvar uvar -> Pretty

pprGenType       sty ty = ppr_ty sty (init_ppr_env sty) tOP_PREC   ty
pprParendGenType sty ty = ppr_ty sty (init_ppr_env sty) tYCON_PREC ty

pprType       	 sty ty = ppr_ty sty (init_ppr_env sty) tOP_PREC   (ty :: Type)
pprParendType 	 sty ty = ppr_ty sty (init_ppr_env sty) tYCON_PREC (ty :: Type)

pprMaybeTy :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
           => PprStyle -> Maybe (GenType tyvar uvar) -> Pretty
pprMaybeTy sty Nothing   = ppChar '*'
pprMaybeTy sty (Just ty) = pprParendGenType sty ty
\end{code}

\begin{code}
ppr_ty :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
       => PprStyle -> PprEnv tyvar uvar bndr occ -> Int
       -> GenType tyvar uvar
       -> Pretty

ppr_ty sty env ctxt_prec (TyVarTy tyvar)
  = ppr_tyvar env tyvar

ppr_ty sty env ctxt_prec (TyConTy tycon usage)
  = ppr sty tycon

ppr_ty sty env ctxt_prec ty@(ForAllTy _ _)
  | showUserishTypes sty = ppr_ty sty env' ctxt_prec body_ty

  | otherwise = ppSep [ ppPStr SLIT("_forall_"), 
		        ppIntersperse pp'SP pp_tyvars,
			ppPStr SLIT("=>"),
			ppr_ty sty env' ctxt_prec body_ty
		      ]
  where
    (tyvars, body_ty) = splitForAllTy ty
    env' 	      = foldl add_tyvar env tyvars
    pp_tyvars	      = map (ppr_tyvar env') tyvars

ppr_ty sty env ctxt_prec (ForAllUsageTy uv uvs ty)
  = panic "ppr_ty:ForAllUsageTy"

ppr_ty sty env ctxt_prec ty@(FunTy (DictTy _ _ _) _ _)
  | showUserishTypes sty
    -- Print a nice looking context  (Eq a, Text b) => ...
  = ppSep [ppBeside (ppr_theta theta) (ppPStr SLIT(" =>")),
	   ppr_ty sty env ctxt_prec body_ty
    ]
  where
    (theta, body_ty) = splitRhoTy ty

    ppr_theta = case sty of { PprInterface -> ppr_theta_2 ; _ -> ppr_theta_1 }

    ppr_theta_1 [ct] = ppr_dict sty env tOP_PREC ct
    ppr_theta_1 cts  = ppParens (ppInterleave ppComma (map (ppr_dict sty env tOP_PREC) cts))

    ppr_theta_2 cts  = ppBesides [ppStr "{{", ppInterleave ppComma (map (ppr_dict sty env tOP_PREC) cts), ppStr "}}"]

ppr_ty sty env ctxt_prec (FunTy ty1 ty2 usage)
    -- We fiddle the precedences passed to left/right branches,
    -- so that right associativity comes out nicely...
  = maybeParen ctxt_prec fUN_PREC
	(ppCat [ppr_ty sty env fUN_PREC ty1,
		ppPStr SLIT("->"),
		ppr_ty sty env tOP_PREC ty2])

ppr_ty sty env ctxt_prec ty@(AppTy _ _)
  = ppr_corner sty env ctxt_prec fun_ty arg_tys
  where
    (fun_ty, arg_tys) = splitAppTy ty

ppr_ty sty env ctxt_prec (SynTy tycon tys expansion)
  | codeStyle sty
	-- always expand types that squeak into C-variable names
  = ppr_ty sty env ctxt_prec expansion

  | otherwise
  = ppBeside
     (ppr_app sty env ctxt_prec (ppr sty tycon) tys)
     (ifPprShowAll sty (ppCat [ppStr " {- expansion:",
			       ppr_ty sty env tOP_PREC expansion,
			       ppStr "-}"]))

ppr_ty sty env ctxt_prec (DictTy clas ty usage)
  = ppr_dict sty env ctxt_prec (clas, ty)

-- Some help functions
ppr_corner sty env ctxt_prec (TyConTy FunTyCon usage) arg_tys
  | length arg_tys == 2
  = ppr_ty sty env ctxt_prec (FunTy ty1 ty2 usage)
  where
    (ty1:ty2:_) = arg_tys

ppr_corner sty env ctxt_prec (TyConTy (TupleTyCon _ _ a) usage) arg_tys
  | not (codeStyle sty) -- no magic in that case
  = --ASSERT(length arg_tys == a)
    (if (length arg_tys /= a) then pprTrace "ppr_corner:" (ppCat [ppInt a, ppInterleave ppComma (map (pprGenType PprDebug) arg_tys)]) else id) $
    ppBesides [ppLparen, arg_tys_w_commas, ppRparen]
  where
    arg_tys_w_commas = ppIntersperse pp'SP (map (ppr_ty sty env tOP_PREC) arg_tys)

ppr_corner sty env ctxt_prec (TyConTy tycon usage) arg_tys
  | not (codeStyle sty) && uniqueOf tycon == listTyConKey
  = ASSERT(length arg_tys == 1)
    ppBesides [ppLbrack, ppr_ty sty env tOP_PREC ty1, ppRbrack]		    
  where
    (ty1:_) = arg_tys

ppr_corner sty env ctxt_prec (TyConTy tycon usage) arg_tys
  = ppr_app sty env ctxt_prec (ppr sty tycon) arg_tys
		      
ppr_corner sty env ctxt_prec (TyVarTy tyvar) arg_tys
  = ppr_app sty env ctxt_prec (ppr_tyvar env tyvar) arg_tys
  

ppr_app sty env ctxt_prec pp_fun []      
  = pp_fun
ppr_app sty env ctxt_prec pp_fun arg_tys 
  = maybeParen ctxt_prec tYCON_PREC (ppCat [pp_fun, arg_tys_w_spaces])
  where
    arg_tys_w_spaces = ppIntersperse ppSP (map (ppr_ty sty env tYCON_PREC) arg_tys)


ppr_dict sty env ctxt_prec (clas, ty)
  = maybeParen ctxt_prec tYCON_PREC
	(ppCat [ppr sty clas, ppr_ty sty env tYCON_PREC ty]) 
\end{code}

This stuff is effectively stubbed out for the time being
(WDP 960425):
\begin{code}
init_ppr_env sty
  = initPprEnv sty b b b b b b b b b b b
  where
    b = panic "PprType:init_ppr_env"

ppr_tyvar env tyvar = ppr (pStyle env) tyvar
ppr_uvar  env uvar  = ppr (pStyle env) uvar

add_tyvar env tyvar = env
add_uvar  env  uvar = env
\end{code}

@ppr_ty@ takes an @Int@ that is the precedence of the context.
The precedence levels are:
\begin{description}
\item[0:] What we start with.
\item[1:] Function application (@FunTys@).
\item[2:] Type constructors.
\end{description}


\begin{code}
tOP_PREC    = (0 :: Int)
fUN_PREC    = (1 :: Int)
tYCON_PREC  = (2 :: Int)

maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = ppParens pretty
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
      _		   -> ppBesides [pp_name, ppStr "{-", pp_u, ppStr "-}"]
  where
    pp_u    = pprUnique uniq
    pp_name = case name of
		Just n  -> ppPStr (getLocalName n)
		Nothing -> case kind of
				TypeKind        -> ppChar 'o'
				BoxedTypeKind   -> ppChar 't'
				UnboxedTypeKind -> ppChar 'u'
				ArrowKind _ _   -> ppChar 'a'
\end{code}

%************************************************************************
%*									*
\subsection[TyCon]{@TyCon@}
%*									*
%************************************************************************

ToDo; all this is suspiciously like getOccName!

\begin{code}
showTyCon :: PprStyle -> TyCon -> String
showTyCon sty tycon = ppShow 80 (pprTyCon sty tycon)

maybe_code sty x
  = if codeStyle sty
    then ppBesides (ppPStr SLIT("Prelude_") : map mangle x)
    else ppStr x
  where
    -- ToDo: really should be in CStrings
    mangle '(' = ppPStr SLIT("Z40") -- decimal ascii #s
    mangle ')' = ppPStr SLIT("Z41")
    mangle '[' = ppPStr SLIT("Z91")
    mangle ']' = ppPStr SLIT("Z93")
    mangle ',' = ppPStr SLIT("Z44")
    mangle '-' = ppPStr SLIT("Zm")
    mangle '>' = ppPStr SLIT("Zg")

pprTyCon :: PprStyle -> TyCon -> Pretty

pprTyCon sty (PrimTyCon _ name _ _) = ppr sty name

pprTyCon sty FunTyCon 		    = maybe_code sty "->"
pprTyCon sty (TupleTyCon _ _ arity) = case arity of
					0 -> maybe_code sty "()"
					2 -> maybe_code sty "(,)"
					3 -> maybe_code sty "(,,)"
					4 -> maybe_code sty "(,,,)"
					5 -> maybe_code sty "(,,,,)"
					n -> maybe_code sty ( "(" ++ nOfThem (n-1) ',' ++ ")" )

pprTyCon sty tycon@(DataTyCon uniq name kind tyvars ctxt cons derivings nd)
  | uniq == listTyConKey
  = maybe_code sty "[]"
  | otherwise
  = ppr sty name

pprTyCon sty (SpecTyCon tc ty_maybes)
  = ppBeside (pprTyCon sty tc)
	     ((if (codeStyle sty) then identToC else ppPStr) tys_stuff)
  where
    tys_stuff = specMaybeTysSuffix ty_maybes

pprTyCon sty (SynTyCon uniq name kind arity tyvars expansion)
  = ppBeside (ppr sty name)
	     (ifPprShowAll sty
		(ppCat [ ppStr " {-", 
			 ppInt arity, 
			 interpp'SP sty tyvars,
			 pprParendGenType sty expansion,
			 ppStr "-}"]))
\end{code}


%************************************************************************
%*									*
\subsection[Class]{@Class@}
%*									*
%************************************************************************

\begin{code}
pprGenClassOp :: Outputable ty => PprStyle -> GenClassOp ty -> Pretty

pprGenClassOp sty op = ppr_class_op sty [] op

ppr_class_op sty tyvars (ClassOp op_name i ty)
  = case sty of
      PprForC 	    -> pp_C
      PprForAsm _ _ -> pp_C
      PprInterface  -> pp_sigd
      PprShowAll    -> pp_sigd
      _		    -> pp_user
  where
    pp_C    = ppPStr op_name
    pp_user = if isLexVarSym op_name && not (isLexSpecialSym op_name)
	      then ppParens pp_C
	      else pp_C
    pp_sigd = ppCat [pp_user, ppPStr SLIT("::"), ppr sty ty]
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

getTypeString :: Type -> [Either OrigName FAST_STRING]

getTypeString ty
  = case (splitAppTy ty) of { (tc, args) ->
    do_tc tc : map do_arg_ty args }
  where
    do_tc (TyConTy tc _) = Left (origName "do_tc" tc)
    do_tc (SynTy _ _ ty) = do_tc ty
    do_tc other = --pprTrace "getTypeString:do_tc:" (pprType PprDebug other) $
		  Right (_PK_ (ppShow 1000 (pprType PprForC other)))

    do_arg_ty (TyConTy tc _) = Left (origName "do_arg_ty" tc)
    do_arg_ty (TyVarTy tv)   = Right (_PK_ (ppShow 80 (ppr PprForC tv)))
    do_arg_ty (SynTy _ _ ty) = do_arg_ty ty
    do_arg_ty other	     = --pprTrace "getTypeString:do_arg_ty:" (pprType PprDebug other) $
			       Right (_PK_ (ppShow 1000 (pprType PprForC other)))

	-- PprForC expands type synonyms as it goes;
	-- it also forces consistent naming of tycons
	-- (e.g., can't have both "(,) a b" and "(a,b)":
	-- must be consistent!

    --------------------------------------------------
    -- tidy: very ad-hoc
    tidy [] = [] -- done

    tidy (' ' : more)
      = case more of
	  ' ' : _	 -> tidy more
	  '-' : '>' : xs -> '-' : '>' : tidy (no_leading_sps xs)
	  other	    	 -> ' ' : tidy more

    tidy (',' : more) = ',' : tidy (no_leading_sps more)

    tidy (x : xs) = x : tidy xs  -- catch all

    no_leading_sps [] = []
    no_leading_sps (' ':xs) = no_leading_sps xs
    no_leading_sps other = other

typeMaybeString :: Maybe Type -> [Either OrigName FAST_STRING]
typeMaybeString Nothing  = [Right SLIT("!")]
typeMaybeString (Just t) = getTypeString t

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
      TyConTy tycon _ -> _UNPK_ (getLocalName tycon)
      SynTy tycon _ _ -> _UNPK_ (getLocalName tycon)
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
  = --pprTrace "addTyVar:" (ppCat [pprUnique u, pprUnique ut]) $
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
	--pprTrace "nmbrTyVar: lookup failed:" (ppCat (ppr PprDebug u : [ppCat [ppr PprDebug x, ppStr "=>", ppr PprDebug tv] | (x,tv) <- ufmToList tvenv])) $
	(nenv, tv)
\end{code}

nmbrTyCon : only called from ``top-level'', if you know what I mean.
\begin{code}
nmbrTyCon tc@FunTyCon		  = returnNmbr tc
nmbrTyCon tc@(TupleTyCon _ _ _)	  = returnNmbr tc
nmbrTyCon tc@(PrimTyCon  _ _ _ _) = returnNmbr tc

nmbrTyCon (DataTyCon u n k tvs theta cons clss nod)
  = --pprTrace "nmbrDataTyCon:" (ppCat (map (ppr PprDebug) tvs)) $
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

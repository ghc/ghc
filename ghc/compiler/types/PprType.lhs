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
	GenClass, 
	GenClassOp, pprGenClassOp
 ) where

import Ubiq
import IdLoop 	-- for paranoia checking
import TyLoop 	-- for paranoia checking
import NameLoop	-- for paranoia checking

-- friends:
-- (PprType can see all the representations it's trying to print)
import Type		( GenType(..), maybeAppTyCon,
			  splitForAllTy, splitSigmaTy, splitRhoTy, splitAppTy )
import TyVar		( GenTyVar(..) )
import TyCon		( TyCon(..), NewOrData )
import Class		( Class(..), GenClass(..),
			  ClassOp(..), GenClassOp(..) )
import Kind		( Kind(..) )

-- others:
import CStrings		( identToC )
import CmdLineOpts	( opt_OmitInterfacePragmas )
import Maybes		( maybeToBool )
import NameTypes	( ShortName, FullName )
import Outputable	( ifPprShowAll, isAvarop, interpp'SP )
import PprStyle		( PprStyle(..), codeStyle, showUserishTypes )
import Pretty
import TysWiredIn	( listTyCon )
import Unique		( pprUnique10, pprUnique )
import Usage		( UVar(..), pprUVar )
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

pprGenType       sty ty = ppr_ty sty (initial_ve sty) tOP_PREC   ty
pprParendGenType sty ty = ppr_ty sty (initial_ve sty) tYCON_PREC ty

pprType       	 sty ty = ppr_ty sty (initial_ve sty) tOP_PREC   (ty :: Type)
pprParendType 	 sty ty = ppr_ty sty (initial_ve sty) tYCON_PREC (ty :: Type)

pprMaybeTy :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
           => PprStyle -> Maybe (GenType tyvar uvar) -> Pretty
pprMaybeTy sty Nothing   = ppChar '*'
pprMaybeTy sty (Just ty) = pprParendGenType sty ty
\end{code}

\begin{code}
ppr_ty :: (Eq tyvar, Outputable tyvar, Eq uvar, Outputable uvar)
       => PprStyle -> VarEnv tyvar uvar -> Int
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
  = ppSep [ppBesides [ppLparen, 
	   	      ppIntersperse pp'SP (map (ppr_dict sty env tOP_PREC) theta),
		      ppRparen],
	   ppPStr SLIT("=>"),
	   ppr_ty sty env ctxt_prec body_ty
    ]
  where
    (theta, body_ty) = splitRhoTy ty

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

ppr_ty PprInterface env ctxt_prec (SynTy tycon tys expansion)
  -- always expand types in an interface
  = ppr_ty PprInterface env ctxt_prec expansion

ppr_ty sty env ctxt_prec (SynTy tycon tys expansion)
  = ppBeside
     (ppr_app sty env ctxt_prec (ppr sty tycon) tys)
     (ifPprShowAll sty (ppCat [ppStr " {- expansion:",
			       ppr_ty sty env tOP_PREC expansion,
			       ppStr "-}"]))

ppr_ty sty env ctxt_prec (DictTy clas ty usage)
  = ppr_dict sty env ctxt_prec (clas, ty)


-- Some help functions
ppr_corner sty env ctxt_prec (TyConTy FunTyCon usage) arg_tys
  = ASSERT(length arg_tys == 2)
    ppr_ty sty env ctxt_prec (FunTy ty1 ty2 usage)
  where
    (ty1:ty2:_) = arg_tys

ppr_corner sty env ctxt_prec (TyConTy (TupleTyCon a) usage) arg_tys
  = ASSERT(length arg_tys == a)
    ppBesides [ppLparen, arg_tys_w_commas, ppRparen]
  where
    arg_tys_w_commas = ppIntersperse pp'SP (map (ppr_ty sty env tOP_PREC) arg_tys)

ppr_corner sty env ctxt_prec (TyConTy tycon usage) arg_tys
  | tycon == listTyCon
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

Nota Bene: we must assign print-names to the forall'd type variables
alphabetically, with the first forall'd variable having the alphabetically
first name.  Reason: so anyone reading the type signature printed without
explicit forall's will be able to reconstruct them in the right order.

\begin{code}
-- Entirely local to this module
data VarEnv tyvar uvar
  = VE	[Pretty] 		-- Tyvar pretty names
	(tyvar -> Pretty)	-- Tyvar lookup function
        [Pretty]		-- Uvar  pretty names
	(uvar -> Pretty)	-- Uvar  lookup function

initial_ve PprForC = VE [] (\tv -> ppChar '*')
			[] (\tv -> ppChar '#')

initial_ve sty = VE tv_pretties (ppr sty)
		    uv_pretties (ppr sty)
  where
    tv_pretties = map (\ c -> ppChar c ) ['a' .. 'h']
		  ++
		  map (\ n -> ppBeside (ppChar 'a') (ppInt n))
		      ([0 .. ] :: [Int])	-- a0 ... aN
    
    uv_pretties = map (\ c -> ppChar c ) ['u' .. 'y']
		  ++
		  map (\ n -> ppBeside (ppChar 'u') (ppInt n))
		      ([0 .. ] :: [Int])	-- u0 ... uN
    

ppr_tyvar (VE _ ppr _ _) tyvar = ppr tyvar
ppr_uvar  (VE _ _ _ ppr) uvar  = ppr uvar

add_tyvar ve@(VE [] _ _ _) tyvar = ve
add_tyvar (VE (tv_pp:tv_supply') tv_ppr uv_supply uv_ppr) tyvar
  = VE tv_supply' tv_ppr' uv_supply uv_ppr
  where
    tv_ppr' tv | tv==tyvar = tv_pp
	       | otherwise = tv_ppr tv

add_uvar ve@(VE _ _ [] _) uvar = ve
add_uvar (VE tv_supply tv_ppr (uv_pp:uv_supply') uv_ppr) uvar
  = VE tv_supply tv_ppr uv_supply' uv_ppr'
  where
    uv_ppr' uv | uv==uvar = uv_pp
	       | otherwise = uv_ppr uv
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
  = ppBesides [pp_name, pprUnique10 uniq]
  where
    pp_name = case name of
		Just n  -> ppr sty n
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

ToDo; all this is suspiciously like getOccurrenceName!

\begin{code}
showTyCon :: PprStyle -> TyCon -> String
showTyCon sty tycon = ppShow 80 (pprTyCon sty tycon)

pprTyCon :: PprStyle -> TyCon -> Pretty

pprTyCon sty FunTyCon 		        = ppStr "(->)"
pprTyCon sty (TupleTyCon arity)	        = ppBeside (ppPStr SLIT("Tuple")) (ppInt arity)
pprTyCon sty (PrimTyCon uniq name kind) = ppr sty name

pprTyCon sty tycon@(DataTyCon uniq kind name tyvars ctxt cons derivings nd)
  = case sty of
      PprDebug   -> pp_tycon_and_uniq
      PprShowAll -> pp_tycon_and_uniq
      _	    	 -> pp_tycon
  where
    pp_tycon_and_uniq = ppBesides [pp_tycon, ppChar '.', pprUnique uniq]
    pp_tycon 	      = ppr sty name

pprTyCon sty (SpecTyCon tc ty_maybes)
  = ppBeside (pprTyCon sty tc)
	     (if (codeStyle sty)
	      then identToC tys_stuff
	      else ppPStr   tys_stuff)
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
      PprInterface  -> ppCat [pp_user, ppPStr SLIT("::"), ppr sty ty]
      PprShowAll    -> ppCat [pp_user, ppPStr SLIT("::"), ppr sty ty]
      _		    -> pp_user
  where
    pp_C    = ppPStr op_name
    pp_user = if isAvarop op_name
	      then ppBesides [ppLparen, pp_C, ppRparen]
	      else pp_C
\end{code}


%************************************************************************
%*									*
\subsection[]{Mumbo jumbo}
%*									*
%************************************************************************

\begin{code}
    -- Shallowly magical; converts a type into something
    -- vaguely close to what can be used in C identifier.
    -- Don't forget to include the module name!!!
getTypeString :: Type -> [FAST_STRING]
getTypeString ty
  | is_prelude_ty = [string]
  | otherwise     = [mod, string]
  where
    string = _PK_ (tidy (ppShow 1000 ppr_t))
    ppr_t  = pprGenType PprForC ty
			-- PprForC expands type synonyms as it goes

    (is_prelude_ty, mod)
      = case (maybeAppTyCon ty) of
	  Nothing -> true_bottom
	  Just (tycon,_) ->
	    if fromPreludeCore tycon
	    then true_bottom
	    else (False, fst (getOrigName tycon))

    true_bottom = (True, panic "getTypeString")

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

typeMaybeString :: Maybe Type -> [FAST_STRING]
typeMaybeString Nothing  = [SLIT("!")]
typeMaybeString (Just t) = getTypeString t

specMaybeTysSuffix :: [Maybe Type] -> FAST_STRING
specMaybeTysSuffix ty_maybes
  = let
	ty_strs  = concat (map typeMaybeString ty_maybes)
	dotted_tys = [ _CONS_ '.' str | str <- ty_strs ]
    in
    _CONCAT_ dotted_tys
\end{code}

========================================================
	INTERFACE STUFF; move it out


\begin{pseudocode}
pprTyCon sty@PprInterface (SynonymTyCon k n a vs exp unabstract) specs
  = ASSERT (null specs)
    let
	lookup_fn   = mk_lookup_tyvar_fn sty vs
	pp_tyvars   = map lookup_fn vs
    in
    ppCat [ppPStr SLIT("type"), ppr sty n, ppIntersperse ppSP pp_tyvars,
	   ppEquals, ppr_ty sty lookup_fn tOP_PREC exp]

pprTyCon sty@PprInterface this_tycon@(DataTyCon k n a vs ctxt cons derivings data_or_new) specs
  = ppHang (ppCat [pp_data_or_new,
		   pprContext sty ctxt,
		   ppr sty n,
		   ppIntersperse ppSP (map lookup_fn vs)])
	   4
	   (ppCat [pp_unabstract_condecls,
		   pp_pragma])
	   -- NB: we do not print deriving info in interfaces
  where
    lookup_fn = mk_lookup_tyvar_fn sty vs

    pp_data_or_new = case data_or_new of
		      DataType -> ppPStr SLIT("data")
		      NewType  -> ppPStr SLIT("newtype")

    yes_we_print_condecls
      = unabstract
    	&& not (null cons)	-- we know what they are
	&& (case (getExportFlag n) of
	      ExportAbs -> False
	      other 	-> True)

    yes_we_print_pragma_condecls
      = not yes_we_print_condecls
	&& not opt_OmitInterfacePragmas
	&& not (null cons)
	&& not (maybeToBool (maybePurelyLocalTyCon this_tycon))
	{- && not (any (dataConMentionsNonPreludeTyCon this_tycon) cons) -}

    yes_we_print_pragma_specs
      = not (null specs)

    pp_unabstract_condecls
      = if yes_we_print_condecls
	then ppCat [ppSP, ppEquals, pp_condecls]
	else ppNil

    pp_pragma_condecls
      = if yes_we_print_pragma_condecls
	then pp_condecls
	else ppNil

    pp_pragma_specs
      = if yes_we_print_pragma_specs
	then pp_specs
	else ppNil

    pp_pragma
      = if (yes_we_print_pragma_condecls || yes_we_print_pragma_specs)
	then ppCat [ppStr "\t{-# GHC_PRAGMA", pp_pragma_condecls, pp_pragma_specs, ppStr "#-}"]
	else ppNil

    pp_condecls
      = let
	    (c:cs) = cons
	in
	ppCat ((ppr_con c) : (map ppr_next_con cs))
      where
	ppr_con con
	  = let
		(_, _, con_arg_tys, _) = dataConSig con
	    in
	    ppCat [pprNonOp PprForUser con, -- the data con's name...
		   ppIntersperse ppSP (map (ppr_ty sty lookup_fn tYCON_PREC) con_arg_tys)]

    	ppr_next_con con = ppCat [ppChar '|', ppr_con con]

    pp_specs
      = ppBesides [ppPStr SLIT("_SPECIALIZE_ "), pp_the_list [
	  ppCat [ppLbrack, ppInterleave ppComma (map pp_maybe ty_maybes), ppRbrack]
	  | ty_maybes <- specs ]]

    pp_the_list [p]    = p
    pp_the_list (p:ps) = ppAbove (ppBeside p ppComma) (pp_the_list ps)

    pp_maybe Nothing   = pp_NONE
    pp_maybe (Just ty) = pprParendGenType sty ty

    pp_NONE = ppPStr SLIT("_N_")

pprTyCon PprInterface (TupleTyCon a) specs
  = ASSERT (null specs)
    ppCat [ ppStr "{- Tuple", ppInt a, ppStr "-}" ]

pprTyCon PprInterface (PrimTyCon k n a kind_fn) specs
  = ASSERT (null specs)
    ppCat [ ppStr "{- data", ppr PprForUser n, ppStr " *built-in* -}" ]





pprIfaceClass :: (Id -> Id) -> IdEnv UnfoldingDetails -> Class -> Pretty

pprIfaceClass better_id_fn inline_env
	(Class k n tyvar super_classes sdsels ops sels defms insts links)
  = let
	sdsel_infos = map (getIdInfo . better_id_fn) sdsels
    in
    ppAboves [ ppCat [ppPStr SLIT("class"), ppr_theta tyvar super_classes,
		      ppr sty n, lookup_fn tyvar,
		      if null sdsel_infos
		      || opt_OmitInterfacePragmas
		      || (any boringIdInfo sdsel_infos)
			-- ToDo: really should be "all bor..."
			-- but then parsing is more tedious,
			-- and this is really as good in practice.
		      then ppNil
		      else pp_sdsel_pragmas (sdsels `zip` sdsel_infos),
		      if (null ops)
		      then ppNil
		      else ppPStr SLIT("where")],
	       ppNest 8  (ppAboves
		 [ ppr_op op (better_id_fn sel) (better_id_fn defm)
		 | (op,sel,defm) <- zip3 ops sels defms]) ]
  where
    lookup_fn = mk_lookup_tyvar_fn sty [tyvar]

    ppr_theta :: TyVar -> [Class] -> Pretty
    ppr_theta tv [] = ppNil
    ppr_theta tv super_classes
      = ppBesides [ppLparen,
		   ppIntersperse pp'SP{-'-} (map ppr_assert super_classes),
		   ppStr ") =>"]
      where
	ppr_assert (Class _ n _ _ _ _ _ _ _ _) = ppCat [ppr sty n, lookup_fn tv]

    pp_sdsel_pragmas sdsels_and_infos
      = ppCat [ppStr "{-# GHC_PRAGMA {-superdicts-}",
	       ppIntersperse pp'SP{-'-}
		 [ppIdInfo sty sdsel False{-NO specs-} better_id_fn inline_env info
		 | (sdsel, info) <- sdsels_and_infos ],
	       ppStr "#-}"]

    ppr_op op opsel_id defm_id
      = let
	    stuff = ppBeside (ppChar '\t') (ppr_class_op sty [tyvar] op)
	in
	if opt_OmitInterfacePragmas
	then stuff
	else ppAbove stuff
		(ppCat [ppStr "\t {-# GHC_PRAGMA", ppAbove pp_opsel pp_defm, ppStr "#-}"])
      where
	pp_opsel = ppCat [ppPStr SLIT("{-meth-}"), ppIdInfo sty opsel_id False{-no specs-} better_id_fn inline_env (getIdInfo opsel_id)]
	pp_defm  = ppCat [ppPStr SLIT("\t\t{-defm-}"), ppIdInfo sty defm_id False{-no specs-} better_id_fn inline_env (getIdInfo defm_id)]
\end{pseudocode}

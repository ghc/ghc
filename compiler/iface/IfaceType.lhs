%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

This module defines interface types and binders

\begin{code}
module IfaceType (
	IfaceType(..), IfaceKind, IfacePredType(..), IfaceTyCon(..),
	IfaceContext, IfaceBndr(..), IfaceTvBndr, IfaceIdBndr, IfaceCoercion,
	ifaceTyConName,

	-- Conversion from Type -> IfaceType
	toIfaceType, toIfacePred, toIfaceContext, 
	toIfaceBndr, toIfaceIdBndr, toIfaceTvBndrs, 
	toIfaceTyCon, toIfaceTyCon_name,

	-- Printing
	pprIfaceType, pprParendIfaceType, pprIfaceContext, 
	pprIfaceIdBndr, pprIfaceTvBndr, pprIfaceTvBndrs, pprIfaceBndrs,
	tOP_PREC, tYCON_PREC, noParens, maybeParen, pprIfaceForAllPart

    ) where

import TypeRep
import TyCon
import Id
import Var
import TysWiredIn
import Name
import BasicTypes
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
		Local (nested) binders
%*									*
%************************************************************************

\begin{code}
data IfaceBndr 		-- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !IfaceIdBndr
  | IfaceTvBndr {-# UNPACK #-} !IfaceTvBndr

type IfaceIdBndr  = (FastString, IfaceType)
type IfaceTvBndr  = (FastString, IfaceKind)

-------------------------------
type IfaceKind     = IfaceType
type IfaceCoercion = IfaceType

data IfaceType
  = IfaceTyVar    FastString			-- Type variable only, not tycon
  | IfaceAppTy    IfaceType IfaceType
  | IfaceForAllTy IfaceTvBndr IfaceType
  | IfacePredTy   IfacePredType
  | IfaceTyConApp IfaceTyCon [IfaceType]	-- Not necessarily saturated
						-- Includes newtypes, synonyms, tuples
  | IfaceFunTy  IfaceType IfaceType

data IfacePredType 	-- NewTypes are handled as ordinary TyConApps
  = IfaceClassP Name [IfaceType]
  | IfaceIParam (IPName OccName) IfaceType
  | IfaceEqPred IfaceType IfaceType

type IfaceContext = [IfacePredType]

data IfaceTyCon 	-- Abbreviations for common tycons with known names
  = IfaceTc Name	-- The common case
  | IfaceIntTc | IfaceBoolTc | IfaceCharTc
  | IfaceListTc | IfacePArrTc
  | IfaceTupTc Boxity Arity 
  | IfaceAnyTc IfaceKind     -- Used for AnyTyCon (see Note [Any Types] in TysPrim)
    	       		     -- other than 'Any :: *' itself
  | IfaceLiftedTypeKindTc | IfaceOpenTypeKindTc | IfaceUnliftedTypeKindTc
  | IfaceUbxTupleKindTc | IfaceArgTypeKindTc 

ifaceTyConName :: IfaceTyCon -> Name
ifaceTyConName IfaceIntTc  	       = intTyConName
ifaceTyConName IfaceBoolTc 	       = boolTyConName
ifaceTyConName IfaceCharTc 	       = charTyConName
ifaceTyConName IfaceListTc 	       = listTyConName
ifaceTyConName IfacePArrTc 	       = parrTyConName
ifaceTyConName (IfaceTupTc bx ar)      = getName (tupleTyCon bx ar)
ifaceTyConName IfaceLiftedTypeKindTc   = liftedTypeKindTyConName
ifaceTyConName IfaceOpenTypeKindTc     = openTypeKindTyConName
ifaceTyConName IfaceUnliftedTypeKindTc = unliftedTypeKindTyConName
ifaceTyConName IfaceUbxTupleKindTc     = ubxTupleKindTyConName
ifaceTyConName IfaceArgTypeKindTc      = argTypeKindTyConName
ifaceTyConName (IfaceTc ext)           = ext
ifaceTyConName (IfaceAnyTc k)          = pprPanic "ifaceTyConName" (ppr k)
	       		    	       	 -- Note [The Name of an IfaceAnyTc]
\end{code}

Note [The Name of an IfaceAnyTc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It isn't easy to get the Name of an IfaceAnyTc in a pure way.  What you
really need to do is to transform it to a TyCon, and get the Name of that.
But doing so needs the monad because there's an IfaceKind inside, and we
need a Kind.

In fact, ifaceTyConName is only used for instances and rules, and we don't
expect to instantiate those at these (internal-ish) Any types, so rather
than solve this potential problem now, I'm going to defer it until it happens!

%************************************************************************
%*									*
		Functions over IFaceTypes
%*									*
%************************************************************************


\begin{code}
splitIfaceSigmaTy :: IfaceType -> ([IfaceTvBndr], IfaceContext, IfaceType)
-- Mainly for printing purposes
splitIfaceSigmaTy ty
  = (tvs,theta,tau)
  where
    (tvs, rho)   = split_foralls ty
    (theta, tau) = split_rho rho

    split_foralls (IfaceForAllTy tv ty) 
	= case split_foralls ty of { (tvs, rho) -> (tv:tvs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceFunTy (IfacePredTy st) ty) 
	= case split_rho ty of { (sts, tau) -> (st:sts, tau) }
    split_rho tau = ([], tau)
\end{code}

%************************************************************************
%*									*
		Pretty-printing
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
tOP_PREC, fUN_PREC, tYCON_PREC :: Int
tOP_PREC    = 0 -- type   in ParseIface.y
fUN_PREC    = 1 -- btype  in ParseIface.y
tYCON_PREC  = 2 -- atype  in ParseIface.y

noParens :: SDoc -> SDoc
noParens pp = pp

maybeParen :: Int -> Int -> SDoc -> SDoc
maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = parens pretty
\end{code}


----------------------------- Printing binders ------------------------------------

\begin{code}
instance Outputable IfaceBndr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <+> pprIfaceTvBndr bndr

pprIfaceBndrs :: [IfaceBndr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceIdBndr :: (FastString, IfaceType) -> SDoc
pprIfaceIdBndr (name, ty) = hsep [ppr name, dcolon, ppr ty]

pprIfaceTvBndr :: IfaceTvBndr -> SDoc
pprIfaceTvBndr (tv, IfaceTyConApp IfaceLiftedTypeKindTc []) 
  = ppr tv
pprIfaceTvBndr (tv, kind) = parens (ppr tv <> dcolon <> ppr kind)
pprIfaceTvBndrs :: [IfaceTvBndr] -> SDoc
pprIfaceTvBndrs tyvars = hsep (map pprIfaceTvBndr tyvars)
\end{code}

----------------------------- Printing IfaceType ------------------------------------

\begin{code}
---------------------------------
instance Outputable IfaceType where
  ppr ty = pprIfaceType ty

pprIfaceType, pprParendIfaceType ::IfaceType -> SDoc
pprIfaceType       = ppr_ty tOP_PREC
pprParendIfaceType = ppr_ty tYCON_PREC


ppr_ty :: Int -> IfaceType -> SDoc
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = ppr_tc_app ctxt_prec tc tys
ppr_ty _         (IfacePredTy st)       = ppr st

	-- Function types
ppr_ty ctxt_prec (IfaceFunTy ty1 ty2)
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen ctxt_prec fUN_PREC $
    sep (ppr_ty fUN_PREC ty1 : ppr_fun_tail ty2)
  where
    ppr_fun_tail (IfaceFunTy ty1 ty2) 
      = (arrow <+> ppr_ty fUN_PREC ty1) : ppr_fun_tail ty2
    ppr_fun_tail other_ty
      = [arrow <+> pprIfaceType other_ty]

ppr_ty ctxt_prec (IfaceAppTy ty1 ty2)
  = maybeParen ctxt_prec tYCON_PREC $
    ppr_ty fUN_PREC ty1 <+> pprParendIfaceType ty2

ppr_ty ctxt_prec ty@(IfaceForAllTy _ _)
  = maybeParen ctxt_prec fUN_PREC (pprIfaceForAllPart tvs theta (pprIfaceType tau))
 where		
    (tvs, theta, tau) = splitIfaceSigmaTy ty
    
-------------------
pprIfaceForAllPart :: [IfaceTvBndr] -> IfaceContext -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt doc 
  = sep [ppr_tvs, pprIfaceContext ctxt, doc]
  where
    ppr_tvs | null tvs  = empty
	    | otherwise = ptext (sLit "forall") <+> pprIfaceTvBndrs tvs <> dot

-------------------
ppr_tc_app :: Int -> IfaceTyCon -> [IfaceType] -> SDoc
ppr_tc_app _         tc 	 []   = ppr_tc tc
ppr_tc_app _         IfaceListTc [ty] = brackets   (pprIfaceType ty)
ppr_tc_app _         IfacePArrTc [ty] = pabrackets (pprIfaceType ty)
ppr_tc_app _         (IfaceTupTc bx arity) tys
  | arity == length tys 
  = tupleParens bx (sep (punctuate comma (map pprIfaceType tys)))
ppr_tc_app ctxt_prec tc tys 
  = maybeParen ctxt_prec tYCON_PREC 
	       (sep [ppr_tc tc, nest 4 (sep (map pprParendIfaceType tys))])

ppr_tc :: IfaceTyCon -> SDoc
-- Wrap infix type constructors in parens
ppr_tc tc@(IfaceTc ext_nm) = parenSymOcc (getOccName ext_nm) (ppr tc)
ppr_tc tc		   = ppr tc

-------------------
instance Outputable IfacePredType where
	-- Print without parens
  ppr (IfaceEqPred ty1 ty2)= hsep [ppr ty1, ptext (sLit "~"), ppr ty2]
  ppr (IfaceIParam ip ty)  = hsep [ppr ip, dcolon, ppr ty]
  ppr (IfaceClassP cls ts) = parenSymOcc (getOccName cls) (ppr cls)
			     <+> sep (map pprParendIfaceType ts)

instance Outputable IfaceTyCon where
  ppr (IfaceAnyTc k) = ptext (sLit "Any") <> pprParendIfaceType k
      		       	     -- We can't easily get the Name of an IfaceAnyTc
			     -- (see Note [The Name of an IfaceAnyTc])
			     -- so we fake it.  It's only for debug printing!
  ppr other_tc       = ppr (ifaceTyConName other_tc)

-------------------
pprIfaceContext :: IfaceContext -> SDoc
-- Prints "(C a, D b) =>", including the arrow
pprIfaceContext []     = empty
pprIfaceContext theta = ppr_preds theta <+> darrow

ppr_preds :: [IfacePredType] -> SDoc
ppr_preds [pred] = ppr pred	-- No parens
ppr_preds preds  = parens (sep (punctuate comma (map ppr preds))) 
			 
-------------------
pabrackets :: SDoc -> SDoc
pabrackets p = ptext (sLit "[:") <> p <> ptext (sLit ":]")
\end{code}

%************************************************************************
%*									*
	Conversion from Type to IfaceType
%*									*
%************************************************************************

\begin{code}
----------------
toIfaceTvBndr :: TyVar -> (FastString, IfaceType)
toIfaceTvBndr tyvar   = (occNameFS (getOccName tyvar), toIfaceKind (tyVarKind tyvar))
toIfaceIdBndr :: Id -> (FastString, IfaceType)
toIfaceIdBndr id      = (occNameFS (getOccName id),    toIfaceType (idType id))
toIfaceTvBndrs :: [TyVar] -> [(FastString, IfaceType)]
toIfaceTvBndrs tyvars = map toIfaceTvBndr tyvars

toIfaceBndr :: Var -> IfaceBndr
toIfaceBndr var
  | isId var  = IfaceIdBndr (toIfaceIdBndr var)
  | otherwise = IfaceTvBndr (toIfaceTvBndr var)

toIfaceKind :: Type -> IfaceType
toIfaceKind = toIfaceType

---------------------
toIfaceType :: Type -> IfaceType
-- Synonyms are retained in the interface type
toIfaceType (TyVarTy tv) =
  IfaceTyVar (occNameFS (getOccName tv))
toIfaceType (AppTy t1 t2) =
  IfaceAppTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (FunTy t1 t2) =
  IfaceFunTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (TyConApp tc tys) =
  IfaceTyConApp (toIfaceTyCon tc) (toIfaceTypes tys)
toIfaceType (ForAllTy tv t) =
  IfaceForAllTy (toIfaceTvBndr tv) (toIfaceType t)
toIfaceType (PredTy st) =
  IfacePredTy (toIfacePred st)

----------------
-- A little bit of (perhaps optional) trickiness here.  When
-- compiling Data.Tuple, the tycons are not TupleTyCons, although
-- they have a wired-in name.  But we'd like to dump them into the Iface
-- as a tuple tycon, to save lookups when reading the interface
-- Hence a tuple tycon may 'miss' in toIfaceTyCon, but then
-- toIfaceTyCon_name will still catch it.

toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon tc 
  | isTupleTyCon tc = IfaceTupTc (tupleTyConBoxity tc) (tyConArity tc)
  | isAnyTyCon tc   = IfaceAnyTc (toIfaceKind (tyConKind tc))
  | otherwise	    = toIfaceTyCon_name (tyConName tc)

toIfaceTyCon_name :: Name -> IfaceTyCon
toIfaceTyCon_name nm
  | Just (ATyCon tc) <- wiredInNameTyThing_maybe nm
  = toIfaceWiredInTyCon tc nm
  | otherwise
  = IfaceTc nm

toIfaceWiredInTyCon :: TyCon -> Name -> IfaceTyCon
toIfaceWiredInTyCon tc nm
  | isTupleTyCon tc                 = IfaceTupTc  (tupleTyConBoxity tc) (tyConArity tc)
  | isAnyTyCon tc                   = IfaceAnyTc (toIfaceKind (tyConKind tc))
  | nm == intTyConName              = IfaceIntTc
  | nm == boolTyConName             = IfaceBoolTc 
  | nm == charTyConName             = IfaceCharTc 
  | nm == listTyConName             = IfaceListTc 
  | nm == parrTyConName             = IfacePArrTc 
  | nm == liftedTypeKindTyConName   = IfaceLiftedTypeKindTc
  | nm == unliftedTypeKindTyConName = IfaceUnliftedTypeKindTc
  | nm == openTypeKindTyConName     = IfaceOpenTypeKindTc
  | nm == argTypeKindTyConName      = IfaceArgTypeKindTc
  | nm == ubxTupleKindTyConName     = IfaceUbxTupleKindTc
  | otherwise		            = IfaceTc nm

----------------
toIfaceTypes :: [Type] -> [IfaceType]
toIfaceTypes ts = map toIfaceType ts

----------------
toIfacePred :: PredType -> IfacePredType
toIfacePred (ClassP cls ts) = 
  IfaceClassP (getName cls) (toIfaceTypes ts)
toIfacePred (IParam ip t) = 
  IfaceIParam (mapIPName getOccName ip) (toIfaceType t)
toIfacePred (EqPred ty1 ty2) =
  IfaceEqPred (toIfaceType ty1) (toIfaceType ty2)

----------------
toIfaceContext :: ThetaType -> IfaceContext
toIfaceContext cs = map toIfacePred cs
\end{code}


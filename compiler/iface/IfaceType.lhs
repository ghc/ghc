%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

This module defines interface types and binders

\begin{code}
module IfaceType (
        IfExtName, IfLclName,

        IfaceType(..), IfacePredType, IfaceKind, IfaceTyCon(..), IfaceCoCon(..),
        IfaceTyLit(..),
        IfaceContext, IfaceBndr(..), IfaceTvBndr, IfaceIdBndr, IfaceCoercion,

        -- Conversion from Type -> IfaceType
        toIfaceType, toIfaceKind, toIfaceContext,
        toIfaceBndr, toIfaceIdBndr, toIfaceTvBndrs,
        toIfaceTyCon, toIfaceTyCon_name,

        -- Conversion from Coercion -> IfaceType
        coToIfaceType,

        -- Printing
        pprIfaceType, pprParendIfaceType, pprIfaceContext,
        pprIfaceIdBndr, pprIfaceTvBndr, pprIfaceTvBndrs, pprIfaceBndrs,
        tOP_PREC, tYCON_PREC, noParens, maybeParen, pprIfaceForAllPart

    ) where

import Coercion
import TypeRep hiding( maybeParen )
import Unique( hasKey )
import TyCon
import CoAxiom
import Id
import Var
import TysWiredIn
import TysPrim
import PrelNames( funTyConKey )
import Name
import BasicTypes
import Outputable
import FastString
\end{code}

%************************************************************************
%*                                                                      *
                Local (nested) binders
%*                                                                      *
%************************************************************************

\begin{code}
type IfLclName = FastString     -- A local name in iface syntax

type IfExtName = Name   -- An External or WiredIn Name can appear in IfaceSyn
                        -- (However Internal or System Names never should)

data IfaceBndr          -- Local (non-top-level) binders
  = IfaceIdBndr {-# UNPACK #-} !IfaceIdBndr
  | IfaceTvBndr {-# UNPACK #-} !IfaceTvBndr

type IfaceIdBndr  = (IfLclName, IfaceType)
type IfaceTvBndr  = (IfLclName, IfaceKind)

-------------------------------
type IfaceKind     = IfaceType
type IfaceCoercion = IfaceType

data IfaceType     -- A kind of universal type, used for types, kinds, and coercions
  = IfaceTyVar    IfLclName               -- Type/coercion variable only, not tycon
  | IfaceAppTy    IfaceType IfaceType
  | IfaceFunTy    IfaceType IfaceType
  | IfaceForAllTy IfaceTvBndr IfaceType
  | IfaceTyConApp IfaceTyCon [IfaceType]  -- Not necessarily saturated
                                          -- Includes newtypes, synonyms, tuples
  | IfaceCoConApp IfaceCoCon [IfaceType]  -- Always saturated
  | IfaceLitTy IfaceTyLit

type IfacePredType = IfaceType
type IfaceContext = [IfacePredType]

data IfaceTyLit
  = IfaceNumTyLit Integer
  | IfaceStrTyLit FastString

-- Encodes type constructors, kind constructors
-- coercion constructors, the lot
newtype IfaceTyCon = IfaceTc { ifaceTyConName :: IfExtName }

  -- Coercion constructors
data IfaceCoCon
  = IfaceCoAx IfExtName Int -- Int is 0-indexed branch number
  | IfaceReflCo    | IfaceUnsafeCo  | IfaceSymCo
  | IfaceTransCo   | IfaceInstCo
  | IfaceNthCo Int | IfaceLRCo LeftOrRight
\end{code}

%************************************************************************
%*                                                                      *
                Functions over IFaceTypes
%*                                                                      *
%************************************************************************


\begin{code}
splitIfaceSigmaTy :: IfaceType -> ([IfaceTvBndr], [IfacePredType], IfaceType)
-- Mainly for printing purposes
splitIfaceSigmaTy ty
  = (tvs, theta, tau)
  where
    (tvs,   rho)   = split_foralls ty
    (theta, tau)   = split_rho rho

    split_foralls (IfaceForAllTy tv ty)
        = case split_foralls ty of { (tvs, rho) -> (tv:tvs, rho) }
    split_foralls rho = ([], rho)

    split_rho (IfaceFunTy ty1 ty2)
      | isIfacePredTy ty1 = case split_rho ty2 of { (ps, tau) -> (ty1:ps, tau) }
    split_rho tau = ([], tau)
\end{code}

%************************************************************************
%*                                                                      *
                Pretty-printing
%*                                                                      *
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
  | otherwise              = parens pretty
\end{code}


----------------------------- Printing binders ------------------------------------

\begin{code}
instance Outputable IfaceBndr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <+> pprIfaceTvBndr bndr

pprIfaceBndrs :: [IfaceBndr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceIdBndr :: (IfLclName, IfaceType) -> SDoc
pprIfaceIdBndr (name, ty) = hsep [ppr name, dcolon, ppr ty]

pprIfaceTvBndr :: IfaceTvBndr -> SDoc
pprIfaceTvBndr (tv, IfaceTyConApp tc [])
  | ifaceTyConName tc == liftedTypeKindTyConName = ppr tv
pprIfaceTvBndr (tv, kind) = parens (ppr tv <> dcolon <> ppr kind)

pprIfaceTvBndrs :: [IfaceTvBndr] -> SDoc
pprIfaceTvBndrs tyvars = sep (map pprIfaceTvBndr tyvars)
\end{code}

----------------------------- Printing IfaceType ------------------------------------

\begin{code}
---------------------------------
instance Outputable IfaceType where
  ppr ty = pprIfaceType ty

pprIfaceType, pprParendIfaceType ::IfaceType -> SDoc
pprIfaceType       = ppr_ty tOP_PREC
pprParendIfaceType = ppr_ty tYCON_PREC

isIfacePredTy :: IfaceType -> Bool
isIfacePredTy _  = False
-- FIXME: fix this to print iface pred tys correctly
-- isIfacePredTy ty = isConstraintKind (ifaceTypeKind ty)

ppr_ty :: Int -> IfaceType -> SDoc
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = ppr_tc_app ctxt_prec tc tys

ppr_ty _ (IfaceLitTy n) = ppr_tylit n

ppr_ty ctxt_prec (IfaceCoConApp tc tys)
  = maybeParen ctxt_prec tYCON_PREC
               (sep [ppr tc, nest 4 (sep (map pprParendIfaceType tys))])

        -- Function types
ppr_ty ctxt_prec (IfaceFunTy ty1 ty2)
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen ctxt_prec fUN_PREC $
    sep (ppr_ty fUN_PREC ty1 : ppr_fun_tail ty2)
  where
    arr | isIfacePredTy ty1 = darrow
        | otherwise         = arrow

    ppr_fun_tail (IfaceFunTy ty1 ty2)
      = (arr <+> ppr_ty fUN_PREC ty1) : ppr_fun_tail ty2
    ppr_fun_tail other_ty
      = [arr <+> pprIfaceType other_ty]

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
ppr_tc_app _         tc          []   = ppr_tc tc


ppr_tc_app _         (IfaceTc n) [ty] | n == listTyConName = brackets (pprIfaceType ty)
ppr_tc_app _         (IfaceTc n) [ty] | n == parrTyConName = paBrackets (pprIfaceType ty)
ppr_tc_app _         (IfaceTc n) tys
  | Just (ATyCon tc) <- wiredInNameTyThing_maybe n
  , Just sort <- tyConTuple_maybe tc
  , tyConArity tc == length tys
  = tupleParens sort (sep (punctuate comma (map pprIfaceType tys)))
ppr_tc_app ctxt_prec tc tys
  = maybeParen ctxt_prec tYCON_PREC
               (sep [ppr_tc tc, nest 4 (sep (map pprParendIfaceType tys))])

ppr_tc :: IfaceTyCon -> SDoc
-- Wrap infix type constructors in parens
ppr_tc tc = wrap (ifaceTyConName tc) (ppr tc)
  where
  -- The kind * does not get wrapped in parens.
  wrap name | name == liftedTypeKindTyConName = id
  wrap name                                   = parenSymOcc (getOccName name)

ppr_tylit :: IfaceTyLit -> SDoc
ppr_tylit (IfaceNumTyLit n) = integer n
ppr_tylit (IfaceStrTyLit n) = text (show n)

-------------------
instance Outputable IfaceTyCon where
  ppr = ppr . ifaceTyConName

instance Outputable IfaceCoCon where
  ppr (IfaceCoAx n i)  = ppr n <> brackets (ppr i)
  ppr IfaceReflCo      = ptext (sLit "Refl")
  ppr IfaceUnsafeCo    = ptext (sLit "Unsafe")
  ppr IfaceSymCo       = ptext (sLit "Sym")
  ppr IfaceTransCo     = ptext (sLit "Trans")
  ppr IfaceInstCo      = ptext (sLit "Inst")
  ppr (IfaceNthCo d)   = ptext (sLit "Nth:") <> int d
  ppr (IfaceLRCo lr)   = ppr lr

instance Outputable IfaceTyLit where
  ppr = ppr_tylit

-------------------
pprIfaceContext :: IfaceContext -> SDoc
-- Prints "(C a, D b) =>", including the arrow
pprIfaceContext []    = empty
pprIfaceContext theta = ppr_preds theta <+> darrow

ppr_preds :: [IfacePredType] -> SDoc
ppr_preds [pred] = ppr pred    -- No parens
ppr_preds preds  = parens (sep (punctuate comma (map ppr preds)))
\end{code}

%************************************************************************
%*                                                                      *
        Conversion from Type to IfaceType
%*                                                                      *
%************************************************************************

\begin{code}
----------------
toIfaceTvBndr :: TyVar -> (IfLclName, IfaceType)
toIfaceTvBndr tyvar   = (occNameFS (getOccName tyvar), toIfaceKind (tyVarKind tyvar))
toIfaceIdBndr :: Id -> (IfLclName, IfaceType)
toIfaceIdBndr id      = (occNameFS (getOccName id),    toIfaceType (idType id))
toIfaceTvBndrs :: [TyVar] -> [(IfLclName, IfaceType)]
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
toIfaceType (TyVarTy tv)      = IfaceTyVar (toIfaceTyVar tv)
toIfaceType (AppTy t1 t2)     = IfaceAppTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (FunTy t1 t2)     = IfaceFunTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (TyConApp tc tys) = IfaceTyConApp (toIfaceTyCon tc) (toIfaceTypes tys)
toIfaceType (LitTy n)         = IfaceLitTy (toIfaceTyLit n)
toIfaceType (ForAllTy tv t)   = IfaceForAllTy (toIfaceTvBndr tv) (toIfaceType t)

toIfaceTyVar :: TyVar -> FastString
toIfaceTyVar = occNameFS . getOccName

toIfaceCoVar :: CoVar -> FastString
toIfaceCoVar = occNameFS . getOccName

----------------
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon = toIfaceTyCon_name . tyConName

toIfaceTyCon_name :: Name -> IfaceTyCon
toIfaceTyCon_name = IfaceTc

toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceTyLit (NumTyLit x) = IfaceNumTyLit x
toIfaceTyLit (StrTyLit x) = IfaceStrTyLit x

----------------
toIfaceTypes :: [Type] -> [IfaceType]
toIfaceTypes ts = map toIfaceType ts

----------------
toIfaceContext :: ThetaType -> IfaceContext
toIfaceContext = toIfaceTypes

----------------
coToIfaceType :: Coercion -> IfaceType
coToIfaceType (Refl ty)             = IfaceCoConApp IfaceReflCo [toIfaceType ty]
coToIfaceType (TyConAppCo tc cos)
  | tc `hasKey` funTyConKey
  , [arg,res] <- cos                = IfaceFunTy (coToIfaceType arg) (coToIfaceType res)
  | otherwise                       = IfaceTyConApp (toIfaceTyCon tc)
                                                    (map coToIfaceType cos)
coToIfaceType (AppCo co1 co2)       = IfaceAppTy    (coToIfaceType co1)
                                                    (coToIfaceType co2)
coToIfaceType (ForAllCo v co)       = IfaceForAllTy (toIfaceTvBndr v)
                                                    (coToIfaceType co)
coToIfaceType (CoVarCo cv)          = IfaceTyVar  (toIfaceCoVar cv)
coToIfaceType (AxiomInstCo con ind cos)
                                    = IfaceCoConApp (coAxiomToIfaceType con ind)
                                                    (map coToIfaceType cos)
coToIfaceType (UnsafeCo ty1 ty2)    = IfaceCoConApp IfaceUnsafeCo
                                                    [ toIfaceType ty1
                                                    , toIfaceType ty2 ]
coToIfaceType (SymCo co)            = IfaceCoConApp IfaceSymCo
                                                    [ coToIfaceType co ]
coToIfaceType (TransCo co1 co2)     = IfaceCoConApp IfaceTransCo
                                                    [ coToIfaceType co1
                                                    , coToIfaceType co2 ]
coToIfaceType (NthCo d co)          = IfaceCoConApp (IfaceNthCo d)
                                                    [ coToIfaceType co ]
coToIfaceType (LRCo lr co)          = IfaceCoConApp (IfaceLRCo lr)
                                                    [ coToIfaceType co ]
coToIfaceType (InstCo co ty)        = IfaceCoConApp IfaceInstCo
                                                    [ coToIfaceType co
                                                    , toIfaceType ty ]

coAxiomToIfaceType :: CoAxiom br -> Int -> IfaceCoCon
coAxiomToIfaceType con ind = IfaceCoAx (coAxiomName con) ind
\end{code}


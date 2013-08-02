%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

This module defines interface types and binders

\begin{code}
module IfaceType (
        IfExtName, IfLclName,

        IfaceType(..), IfacePredType, IfaceKind, IfaceTyCon(..), IfaceCoercion(..),
        IfaceTyLit(..),
        IfaceContext, IfaceBndr(..), IfaceTvBndr, IfaceIdBndr, 

        -- Conversion from Type -> IfaceType
        toIfaceType, toIfaceKind, toIfaceContext,
        toIfaceBndr, toIfaceIdBndr, toIfaceTvBndrs,
        toIfaceTyCon, toIfaceTyCon_name,

        -- Conversion from Coercion -> IfaceCoercion
        toIfaceCoercion,

        -- Printing
        pprIfaceType, pprParendIfaceType, pprIfaceContext,
        pprIfaceIdBndr, pprIfaceTvBndr, pprIfaceTvBndrs, pprIfaceTvBndrsRoles,
        pprIfaceBndrs,
        tOP_PREC, tYCON_PREC, noParens, maybeParen, pprIfaceForAllPart,
        pprIfaceCoercion, pprParendIfaceCoercion

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
import Binary
import Outputable
import FastString

import Control.Monad
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

data IfaceType     -- A kind of universal type, used for types and kinds
  = IfaceTyVar    IfLclName               -- Type/coercion variable only, not tycon
  | IfaceAppTy    IfaceType IfaceType
  | IfaceFunTy    IfaceType IfaceType
  | IfaceForAllTy IfaceTvBndr IfaceType
  | IfaceTyConApp IfaceTyCon [IfaceType]  -- Not necessarily saturated
                                          -- Includes newtypes, synonyms, tuples
  | IfaceLitTy IfaceTyLit

type IfacePredType = IfaceType
type IfaceContext = [IfacePredType]

data IfaceTyLit
  = IfaceNumTyLit Integer
  | IfaceStrTyLit FastString

-- Encodes type constructors, kind constructors
-- coercion constructors, the lot
newtype IfaceTyCon = IfaceTc { ifaceTyConName :: IfExtName }

data IfaceCoercion
  = IfaceReflCo      Role IfaceType
  | IfaceFunCo       Role IfaceCoercion IfaceCoercion
  | IfaceTyConAppCo  Role IfaceTyCon [IfaceCoercion]
  | IfaceAppCo       IfaceCoercion IfaceCoercion
  | IfaceForAllCo    IfaceTvBndr IfaceCoercion
  | IfaceCoVarCo     IfLclName
  | IfaceAxiomInstCo IfExtName BranchIndex [IfaceCoercion]
  | IfaceUnivCo      Role IfaceType IfaceType
  | IfaceSymCo       IfaceCoercion
  | IfaceTransCo     IfaceCoercion IfaceCoercion
  | IfaceNthCo       Int IfaceCoercion
  | IfaceLRCo        LeftOrRight IfaceCoercion
  | IfaceInstCo      IfaceCoercion IfaceType
  | IfaceSubCo       IfaceCoercion
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

pprIfaceTvBndrsRoles :: [IfaceTvBndr] -> [Role] -> SDoc
pprIfaceTvBndrsRoles tyvars roles = sep (zipWith ppr_bndr_role tyvars roles)
  where
    ppr_bndr_role bndr role = pprIfaceTvBndr bndr <> char '@' <> ppr role

instance Binary IfaceBndr where
    put_ bh (IfaceIdBndr aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (IfaceTvBndr ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (IfaceIdBndr aa)
              _ -> do ab <- get bh
                      return (IfaceTvBndr ab)
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
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = ppr_tc_app ppr_ty ctxt_prec tc tys

ppr_ty _ (IfaceLitTy n) = ppr_tylit n

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
-- needs to handle type contexts and coercion contexts, hence the
-- generality
pprIfaceForAllPart :: Outputable a => [IfaceTvBndr] -> [a] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt doc
  = sep [ppr_tvs, pprIfaceContext ctxt, doc]
  where
    ppr_tvs | null tvs  = empty
            | otherwise = ptext (sLit "forall") <+> pprIfaceTvBndrs tvs <> dot

-------------------
ppr_tc_app :: (Int -> a -> SDoc) -> Int -> IfaceTyCon -> [a] -> SDoc
ppr_tc_app _  _         tc          []   = ppr_tc tc


ppr_tc_app pp _         (IfaceTc n) [ty]
  | n == listTyConName
  = brackets (pp tOP_PREC ty)
  | n == parrTyConName
  = paBrackets (pp tOP_PREC ty)
ppr_tc_app pp _         (IfaceTc n) tys
  | Just (ATyCon tc) <- wiredInNameTyThing_maybe n
  , Just sort <- tyConTuple_maybe tc
  , tyConArity tc == length tys
  = tupleParens sort (sep (punctuate comma (map (pp tOP_PREC) tys)))
ppr_tc_app pp ctxt_prec tc tys
  = maybeParen ctxt_prec tYCON_PREC
               (sep [ppr_tc tc, nest 4 (sep (map (pp tYCON_PREC) tys))])

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

pprIfaceCoercion, pprParendIfaceCoercion :: IfaceCoercion -> SDoc
pprIfaceCoercion = ppr_co tOP_PREC
pprParendIfaceCoercion = ppr_co tYCON_PREC

ppr_co :: Int -> IfaceCoercion -> SDoc
ppr_co _         (IfaceReflCo r ty) = angleBrackets (ppr ty) <> ppr_role r
ppr_co ctxt_prec (IfaceFunCo r co1 co2)
  = maybeParen ctxt_prec fUN_PREC $
    sep (ppr_co fUN_PREC co1 : ppr_fun_tail co2)
  where
    ppr_fun_tail (IfaceFunCo r co1 co2)
      = (arrow <> ppr_role r <+> ppr_co fUN_PREC co1) : ppr_fun_tail co2
    ppr_fun_tail other_co
      = [arrow <> ppr_role r <+> pprIfaceCoercion other_co]

ppr_co _         (IfaceTyConAppCo r tc cos)
  = parens (ppr_tc_app ppr_co tOP_PREC tc cos) <> ppr_role r
ppr_co ctxt_prec (IfaceAppCo co1 co2)
  = maybeParen ctxt_prec tYCON_PREC $
    ppr_co fUN_PREC co1 <+> pprParendIfaceCoercion co2
ppr_co ctxt_prec co@(IfaceForAllCo _ _)
  = maybeParen ctxt_prec fUN_PREC (sep [ppr_tvs, pprIfaceCoercion inner_co])
  where
    (tvs, inner_co) = split_co co
    ppr_tvs = ptext (sLit "forall") <+> pprIfaceTvBndrs tvs <> dot

    split_co (IfaceForAllCo tv co')
      = let (tvs, co'') = split_co co' in (tv:tvs,co'')
    split_co co' = ([], co')

ppr_co _         (IfaceCoVarCo covar)       = ppr covar

ppr_co ctxt_prec (IfaceUnivCo r ty1 ty2)
  = maybeParen ctxt_prec tYCON_PREC $
    ptext (sLit "UnivCo") <+> ppr r <+>
    pprParendIfaceType ty1 <+> pprParendIfaceType ty2

ppr_co ctxt_prec (IfaceInstCo co ty)
  = maybeParen ctxt_prec tYCON_PREC $
    ptext (sLit "Inst") <+> pprParendIfaceCoercion co <+> pprParendIfaceType ty

ppr_co ctxt_prec co
  = ppr_special_co ctxt_prec doc cos
  where (doc, cos) = case co of
                     { IfaceAxiomInstCo n i cos -> (ppr n <> brackets (ppr i), cos)
                     ; IfaceSymCo co            -> (ptext (sLit "Sym"), [co])
                     ; IfaceTransCo co1 co2     -> (ptext (sLit "Trans"), [co1,co2])
                     ; IfaceNthCo d co          -> (ptext (sLit "Nth:") <> int d,
                                                    [co])
                     ; IfaceLRCo lr co          -> (ppr lr, [co])
                     ; IfaceSubCo co            -> (ptext (sLit "Sub"), [co])
                     ; _                        -> panic "pprIfaceCo" }

ppr_special_co :: Int -> SDoc -> [IfaceCoercion] -> SDoc
ppr_special_co ctxt_prec doc cos
  = maybeParen ctxt_prec tYCON_PREC
               (sep [doc, nest 4 (sep (map pprParendIfaceCoercion cos))])

ppr_role :: Role -> SDoc
ppr_role r = underscore <> ppr r

-------------------
instance Outputable IfaceTyCon where
  ppr = ppr . ifaceTyConName

instance Outputable IfaceCoercion where
  ppr = pprIfaceCoercion

instance Binary IfaceTyCon where
   put_ bh (IfaceTc ext) = put_ bh ext
   get bh = liftM IfaceTc (get bh)

instance Outputable IfaceTyLit where
  ppr = ppr_tylit

instance Binary IfaceTyLit where
  put_ bh (IfaceNumTyLit n)  = putByte bh 1 >> put_ bh n
  put_ bh (IfaceStrTyLit n)  = putByte bh 2 >> put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         _ -> panic ("get IfaceTyLit " ++ show tag)

-------------------
pprIfaceContext :: Outputable a => [a] -> SDoc
-- Prints "(C a, D b) =>", including the arrow
pprIfaceContext []    = empty
pprIfaceContext theta = ppr_preds theta <+> darrow

ppr_preds :: Outputable a => [a] -> SDoc
ppr_preds [pred] = ppr pred    -- No parens
ppr_preds preds  = parens (sep (punctuate comma (map ppr preds)))

instance Binary IfaceType where
    put_ bh (IfaceForAllTy aa ab) = do
            putByte bh 0
            put_ bh aa
            put_ bh ab
    put_ bh (IfaceTyVar ad) = do
            putByte bh 1
            put_ bh ad
    put_ bh (IfaceAppTy ae af) = do
            putByte bh 2
            put_ bh ae
            put_ bh af
    put_ bh (IfaceFunTy ag ah) = do
            putByte bh 3
            put_ bh ag
            put_ bh ah
    put_ bh (IfaceTyConApp tc tys)
      = do { putByte bh 5; put_ bh tc; put_ bh tys }

    put_ bh (IfaceLitTy n)
      = do { putByte bh 30; put_ bh n }

    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      ab <- get bh
                      return (IfaceForAllTy aa ab)
              1 -> do ad <- get bh
                      return (IfaceTyVar ad)
              2 -> do ae <- get bh
                      af <- get bh
                      return (IfaceAppTy ae af)
              3 -> do ag <- get bh
                      ah <- get bh
                      return (IfaceFunTy ag ah)
              5 -> do { tc <- get bh; tys <- get bh
                      ; return (IfaceTyConApp tc tys) }

              30 -> do n <- get bh
                       return (IfaceLitTy n)

              _  -> panic ("get IfaceType " ++ show h)

instance Binary IfaceCoercion where
  put_ bh (IfaceReflCo a b) = do
          putByte bh 1
          put_ bh a
          put_ bh b
  put_ bh (IfaceFunCo a b c) = do
          putByte bh 2
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceTyConAppCo a b c) = do
          putByte bh 3
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceAppCo a b) = do
          putByte bh 4
          put_ bh a
          put_ bh b
  put_ bh (IfaceForAllCo a b) = do
          putByte bh 5
          put_ bh a
          put_ bh b
  put_ bh (IfaceCoVarCo a) = do
          putByte bh 6
          put_ bh a
  put_ bh (IfaceAxiomInstCo a b c) = do
          putByte bh 7
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceUnivCo a b c) = do
          putByte bh 8
          put_ bh a
          put_ bh b
          put_ bh c
  put_ bh (IfaceSymCo a) = do
          putByte bh 9
          put_ bh a
  put_ bh (IfaceTransCo a b) = do
          putByte bh 10
          put_ bh a
          put_ bh b
  put_ bh (IfaceNthCo a b) = do
          putByte bh 11
          put_ bh a
          put_ bh b
  put_ bh (IfaceLRCo a b) = do
          putByte bh 12
          put_ bh a
          put_ bh b
  put_ bh (IfaceInstCo a b) = do
          putByte bh 13
          put_ bh a
          put_ bh b
  put_ bh (IfaceSubCo a) = do
          putByte bh 14
          put_ bh a
  
  get bh = do
      tag <- getByte bh
      case tag of
           1 -> do a <- get bh
                   b <- get bh
                   return $ IfaceReflCo a b
           2 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceFunCo a b c
           3 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceTyConAppCo a b c
           4 -> do a <- get bh
                   b <- get bh
                   return $ IfaceAppCo a b
           5 -> do a <- get bh
                   b <- get bh
                   return $ IfaceForAllCo a b
           6 -> do a <- get bh
                   return $ IfaceCoVarCo a
           7 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceAxiomInstCo a b c
           8 -> do a <- get bh
                   b <- get bh
                   c <- get bh
                   return $ IfaceUnivCo a b c
           9 -> do a <- get bh
                   return $ IfaceSymCo a
           10-> do a <- get bh
                   b <- get bh
                   return $ IfaceTransCo a b
           11-> do a <- get bh
                   b <- get bh
                   return $ IfaceNthCo a b
           12-> do a <- get bh
                   b <- get bh
                   return $ IfaceLRCo a b
           13-> do a <- get bh
                   b <- get bh
                   return $ IfaceInstCo a b
           14-> do a <- get bh
                   return $ IfaceSubCo a
           _ -> panic ("get IfaceCoercion " ++ show tag)             

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
toIfaceCoercion :: Coercion -> IfaceCoercion
toIfaceCoercion (Refl r ty)         = IfaceReflCo r (toIfaceType ty)
toIfaceCoercion (TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [arg,res] <- cos                = IfaceFunCo r (toIfaceCoercion arg) (toIfaceCoercion res)
  | otherwise                       = IfaceTyConAppCo r (toIfaceTyCon tc)
                                                      (map toIfaceCoercion cos)
toIfaceCoercion (AppCo co1 co2)     = IfaceAppCo  (toIfaceCoercion co1)
                                                  (toIfaceCoercion co2)
toIfaceCoercion (ForAllCo v co)     = IfaceForAllCo (toIfaceTvBndr v)
                                                    (toIfaceCoercion co)
toIfaceCoercion (CoVarCo cv)        = IfaceCoVarCo  (toIfaceCoVar cv)
toIfaceCoercion (AxiomInstCo con ind cos)
                                    = IfaceAxiomInstCo (coAxiomName con) ind
                                                       (map toIfaceCoercion cos)
toIfaceCoercion (UnivCo r ty1 ty2)  = IfaceUnivCo r (toIfaceType ty1)
                                                  (toIfaceType ty2)
toIfaceCoercion (SymCo co)          = IfaceSymCo (toIfaceCoercion co)
toIfaceCoercion (TransCo co1 co2)   = IfaceTransCo (toIfaceCoercion co1)
                                                   (toIfaceCoercion co2)
toIfaceCoercion (NthCo d co)        = IfaceNthCo d (toIfaceCoercion co)
toIfaceCoercion (LRCo lr co)        = IfaceLRCo lr (toIfaceCoercion co)
toIfaceCoercion (InstCo co ty)      = IfaceInstCo (toIfaceCoercion co)
                                                  (toIfaceType ty)
toIfaceCoercion (SubCo co)          = IfaceSubCo (toIfaceCoercion co)

\end{code}


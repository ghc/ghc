{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.GhcUtils
-- Copyright   :  (c) David Waern 2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils for dealing with types from the GHC API
module Haddock.GhcUtils where

import Control.Arrow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Char (isSpace)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Builtin.Names
import GHC.Builtin.Types (liftedRepTy)
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.Type (binderVar, isRuntimeRepVar)
import GHC.Data.FastString
import GHC.Data.StringBuffer (StringBuffer)
import qualified GHC.Data.StringBuffer as S
import GHC.Driver.Session
import GHC.HsToCore.Docs hiding (sigNameNoLoc)
import GHC.Platform (Platform (..))
import GHC.Types.Name
import GHC.Types.SrcLoc (advanceSrcLoc)
import GHC.Types.Var
  ( Specificity
  , TyVarBinder
  , VarBndr (..)
  , isInvisibleForAllTyFlag
  , tyVarKind
  , updateTyVarKind
  )
import GHC.Types.Var.Env (TyVarEnv, elemVarEnv, emptyVarEnv, extendVarEnv)
import GHC.Types.Var.Set (VarSet, emptyVarSet)
import GHC.Utils.FV as FV
import GHC.Utils.Outputable (Outputable, SDocContext, ppr)
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic (panic)

import Haddock.Types (DocName, DocNameI, Interface (..), XRecCond)

moduleString :: Module -> String
moduleString = moduleNameString . moduleName

isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName

-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (IdP (GhcPass p) -> Bool) -> LSig (GhcPass p) -> Maybe (LSig (GhcPass p))
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (IdP (GhcPass p) -> Bool) -> Sig (GhcPass p) -> Maybe (Sig (GhcPass p))
filterSigNames p orig@(SpecSig _ n _ _) = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig _ n _) = ifTrueJust (p $ unLoc n) orig
filterSigNames p (FixSig _ (FixitySig ns_spec ns ty)) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (FixSig noAnn (FixitySig ns_spec filtered ty))
filterSigNames _ orig@(MinimalSig _ _) = Just orig
filterSigNames p (TypeSig _ ns ty) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (TypeSig noAnn filtered ty)
filterSigNames p (ClassOpSig _ is_default ns ty) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (ClassOpSig noAnn is_default filtered ty)
filterSigNames p (PatSynSig _ ns ty) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (PatSynSig noAnn filtered ty)
filterSigNames _ _ = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True = Just
ifTrueJust False = const Nothing

sigName :: LSig GhcRn -> [IdP GhcRn]
sigName (L _ sig) = sigNameNoLoc' emptyOccEnv sig

sigNameNoLoc' :: forall pass w. UnXRec pass => w -> Sig pass -> [IdP pass]
sigNameNoLoc' _ (TypeSig _ ns _) = map (unXRec @pass) ns
sigNameNoLoc' _ (ClassOpSig _ _ ns _) = map (unXRec @pass) ns
sigNameNoLoc' _ (PatSynSig _ ns _) = map (unXRec @pass) ns
sigNameNoLoc' _ (SpecSig _ n _ _) = [unXRec @pass n]
sigNameNoLoc' _ (InlineSig _ n _) = [unXRec @pass n]
sigNameNoLoc' _ (FixSig _ (FixitySig _ ns _)) = map (unXRec @pass) ns
sigNameNoLoc' _ _ = []

-- | Was this signature given by the user?
isUserLSig :: forall p. UnXRec p => LSig p -> Bool
isUserLSig = isUserSig . unXRec @p

isClassD :: HsDecl a -> Bool
isClassD (TyClD _ d) = isClassDecl d
isClassD _ = False

pretty :: Outputable a => SDocContext -> a -> String
pretty sDocContext thing = Outputable.renderWithContext sDocContext (ppr thing)

dataListModule :: Module
dataListModule = mkBaseModule (fsLit "Data.List")

dataTupleModule :: Module
dataTupleModule = mkBaseModule (fsLit "Data.Tuple")

-- ---------------------------------------------------------------------

-- These functions are duplicated from the GHC API, as they must be
-- instantiated at DocNameI instead of (GhcPass _).

-- | Like 'hsTyVarName' from GHC API, but not instantiated at (GhcPass _)
hsTyVarBndrName
  :: forall flag n
   . (XXTyVarBndr n ~ DataConCantHappen, UnXRec n)
  => HsTyVarBndr flag n
  -> IdP n
hsTyVarBndrName (UserTyVar _ _ name) = unXRec @n name
hsTyVarBndrName (KindedTyVar _ _ name _) = unXRec @n name

hsTyVarNameI :: HsTyVarBndr flag DocNameI -> DocName
hsTyVarNameI (UserTyVar _ _ (L _ n)) = n
hsTyVarNameI (KindedTyVar _ _ (L _ n) _) = n

hsLTyVarNameI :: LHsTyVarBndr flag DocNameI -> DocName
hsLTyVarNameI = hsTyVarNameI . unLoc

getConNamesI :: ConDecl DocNameI -> NonEmpty (LocatedN DocName)
getConNamesI ConDeclH98{con_name = name} = pure name
getConNamesI ConDeclGADT{con_names = names} = names

hsSigTypeI :: LHsSigType DocNameI -> LHsType DocNameI
hsSigTypeI = sig_body . unLoc

mkEmptySigType :: LHsType GhcRn -> LHsSigType GhcRn
-- Dubious, because the implicit binders are empty even
-- though the type might have free variables
mkEmptySigType lty@(L loc ty) = L loc $ case ty of
  HsForAllTy
    { hst_tele = HsForAllInvis{hsf_invis_bndrs = bndrs}
    , hst_body = body
    } ->
      HsSig
        { sig_ext = noExtField
        , sig_bndrs =
            HsOuterExplicit
              { hso_xexplicit = noExtField
              , hso_bndrs = bndrs
              }
        , sig_body = body
        }
  _ ->
    HsSig
      { sig_ext = noExtField
      , sig_bndrs = HsOuterImplicit{hso_ximplicit = []}
      , sig_body = lty
      }

mkHsForAllInvisTeleI
  :: [LHsTyVarBndr Specificity DocNameI] -> HsForAllTelescope DocNameI
mkHsForAllInvisTeleI invis_bndrs =
  HsForAllInvis{hsf_xinvis = noExtField, hsf_invis_bndrs = invis_bndrs}

mkHsImplicitSigTypeI :: LHsType DocNameI -> HsSigType DocNameI
mkHsImplicitSigTypeI body =
  HsSig
    { sig_ext = noExtField
    , sig_bndrs = HsOuterImplicit{hso_ximplicit = noExtField}
    , sig_body = body
    }

getGADTConType :: ConDecl DocNameI -> LHsSigType DocNameI
-- The full type of a GADT data constructor We really only get this in
-- order to pretty-print it, and currently only in Haddock's code.  So
-- we are cavalier about locations and extensions, hence the
-- 'undefined's
getGADTConType
  ( ConDeclGADT
      { con_bndrs = L _ outer_bndrs
      , con_mb_cxt = mcxt
      , con_g_args = args
      , con_res_ty = res_ty
      }
    ) =
    noLocA
      ( HsSig
          { sig_ext = noExtField
          , sig_bndrs = outer_bndrs
          , sig_body = theta_ty
          }
      )
    where
      theta_ty
        | Just theta <- mcxt =
            noLocA (HsQualTy{hst_xqual = noAnn, hst_ctxt = theta, hst_body = tau_ty})
        | otherwise =
            tau_ty

      --  tau_ty :: LHsType DocNameI
      tau_ty = case args of
        RecConGADT _ flds -> mkFunTy (noLocA (HsRecTy noAnn (unLoc flds))) res_ty
        PrefixConGADT _ pos_args -> foldr mkFunTy res_ty (map hsScaledThing pos_args)

      mkFunTy :: LHsType DocNameI -> LHsType DocNameI -> LHsType DocNameI
      mkFunTy a b = noLocA (HsFunTy noAnn (HsUnrestrictedArrow noExtField) a b)
getGADTConType (ConDeclH98{}) = panic "getGADTConType"

-- Should only be called on ConDeclGADT

getMainDeclBinderI :: HsDecl DocNameI -> [IdP DocNameI]
getMainDeclBinderI (TyClD _ d) = [tcdNameI d]
getMainDeclBinderI (ValD _ d) =
  case collectHsBindBinders CollNoDictBinders d of
    [] -> []
    (name : _) -> [name]
getMainDeclBinderI (SigD _ d) = sigNameNoLoc' emptyOccEnv d
getMainDeclBinderI (ForD _ (ForeignImport _ name _ _)) = [unLoc name]
getMainDeclBinderI (ForD _ (ForeignExport _ _ _ _)) = []
getMainDeclBinderI _ = []

familyDeclLNameI :: FamilyDecl DocNameI -> LocatedN DocName
familyDeclLNameI (FamilyDecl{fdLName = n}) = n

tyClDeclLNameI :: TyClDecl DocNameI -> LocatedN DocName
tyClDeclLNameI (FamDecl{tcdFam = fd}) = familyDeclLNameI fd
tyClDeclLNameI (SynDecl{tcdLName = ln}) = ln
tyClDeclLNameI (DataDecl{tcdLName = ln}) = ln
tyClDeclLNameI (ClassDecl{tcdLName = ln}) = ln

tcdNameI :: TyClDecl DocNameI -> DocName
tcdNameI = unLoc . tyClDeclLNameI

addClassContext :: Name -> LHsQTyVars GhcRn -> LSig GhcRn -> LSig GhcRn
-- Add the class context to a class-op signature
addClassContext cls tvs0 (L pos (ClassOpSig _ _ lname ltype)) =
  L pos (TypeSig noAnn lname (mkEmptyWildCardBndrs (go_sig_ty ltype)))
  where
    go_sig_ty (L loc (HsSig{sig_bndrs = bndrs, sig_body = ty})) =
      L
        loc
        ( HsSig
            { sig_ext = noExtField
            , sig_bndrs = bndrs
            , sig_body = go_ty ty
            }
        )

    go_ty (L loc (HsForAllTy{hst_tele = tele, hst_body = ty})) =
      L
        loc
        ( HsForAllTy
            { hst_xforall = noExtField
            , hst_tele = tele
            , hst_body = go_ty ty
            }
        )
    go_ty (L loc (HsQualTy{hst_ctxt = ctxt, hst_body = ty})) =
      L
        loc
        ( HsQualTy
            { hst_xqual = noExtField
            , hst_ctxt = add_ctxt ctxt
            , hst_body = ty
            }
        )
    go_ty (L loc ty) =
      L
        loc
        ( HsQualTy
            { hst_xqual = noExtField
            , hst_ctxt = add_ctxt (noLocA [])
            , hst_body = L loc ty
            }
        )

    extra_pred = nlHsTyConApp NotPromoted Prefix cls (lHsQTyVarsToTypes tvs0)

    add_ctxt (L loc preds) = L loc (extra_pred : preds)
addClassContext _ _ sig = sig -- E.g. a MinimalSig is fine

lHsQTyVarsToTypes :: LHsQTyVars GhcRn -> [LHsTypeArg GhcRn]
lHsQTyVarsToTypes tvs =
  [ HsValArg noExtField $ noLocA (HsTyVar noAnn NotPromoted (noLocA (hsLTyVarName tv)))
  | tv <- hsQTvExplicit tvs
  ]

--------------------------------------------------------------------------------

-- * Making abstract declarations

--------------------------------------------------------------------------------

restrictTo :: [Name] -> LHsDecl GhcRn -> LHsDecl GhcRn
restrictTo names (L loc decl) = L loc $ case decl of
  TyClD x d
    | isDataDecl d ->
        TyClD x (d{tcdDataDefn = restrictDataDefn names (tcdDataDefn d)})
  TyClD x d
    | isClassDecl d ->
        TyClD
          x
          ( d
              { tcdSigs = restrictDecls names (tcdSigs d)
              , tcdATs = restrictATs names (tcdATs d)
              }
          )
  _ -> decl

restrictDataDefn :: [Name] -> HsDataDefn GhcRn -> HsDataDefn GhcRn
restrictDataDefn names d = d{dd_cons = restrictDataDefnCons names (dd_cons d)}

restrictDataDefnCons :: [Name] -> DataDefnCons (LConDecl GhcRn) -> DataDefnCons (LConDecl GhcRn)
restrictDataDefnCons names = \case
  DataTypeCons is_type_data cons -> DataTypeCons is_type_data (restrictCons names cons)
  NewTypeCon con -> maybe (DataTypeCons False []) NewTypeCon $ restrictCons names (Just con)

restrictCons :: MonadFail m => [Name] -> m (LConDecl GhcRn) -> m (LConDecl GhcRn)
restrictCons names decls = [L p d | L p (Just d) <- fmap keep <$> decls]
  where
    keep :: ConDecl GhcRn -> Maybe (ConDecl GhcRn)
    keep d
      | any (`elem` names) (unLoc <$> getConNames d) =
          case d of
            ConDeclH98{con_args = con_args'} -> case con_args' of
              PrefixCon{} -> Just d
              RecCon fields
                | all field_avail (unLoc fields) -> Just d
                | otherwise -> Just (d{con_args = PrefixCon [] (field_types $ unLoc fields)})
              -- if we have *all* the field names available, then
              -- keep the record declaration.  Otherwise degrade to
              -- a constructor declaration.  This isn't quite right, but
              -- it's the best we can do.
              InfixCon _ _ -> Just d
            ConDeclGADT{con_g_args = con_args'} -> case con_args' of
              PrefixConGADT{} -> Just d
              RecConGADT _ fields
                | all field_avail (unLoc fields) -> Just d
                | otherwise -> Just (d{con_g_args = PrefixConGADT noExtField (field_types $ unLoc fields)})
      where
        -- see above

        field_avail :: LConDeclField GhcRn -> Bool
        field_avail (L _ (ConDeclField _ fs _ _)) =
          all (\f -> foExt (unLoc f) `elem` names) fs

        field_types flds = [hsUnrestricted t | L _ (ConDeclField _ _ t _) <- flds]
    keep _ = Nothing

restrictDecls :: [Name] -> [LSig GhcRn] -> [LSig GhcRn]
restrictDecls names = mapMaybe (filterLSigNames (`elem` names))

restrictATs :: [Name] -> [LFamilyDecl GhcRn] -> [LFamilyDecl GhcRn]
restrictATs names ats = [at | at <- ats, unLoc (fdLName (unLoc at)) `elem` names]

-------------------------------------------------------------------------------

-- * Parenthesization

-------------------------------------------------------------------------------

-- | Precedence level (inside the 'HsType' AST).
data Precedence
  = -- | precedence of 'type' production in GHC's parser
    PREC_TOP
  | -- | explicit type signature
    PREC_SIG
  | -- | Used for single contexts, eg. ctx => type
    -- (as opposed to (ctx1, ctx2) => type)
    PREC_CTX
  | -- | precedence of 'btype' production in GHC's parser
    -- (used for LH arg of (->))
    PREC_FUN
  | -- | arg of any infix operator
    -- (we don't keep have fixity info)
    PREC_OP
  | -- | arg of type application: always parenthesize unless atomic
    PREC_CON
  deriving (Eq, Ord)

-- | Add in extra 'HsParTy' where needed to ensure that what would be printed
-- out using 'ppr' has enough parentheses to be re-parsed properly.
--
-- We cannot add parens that may be required by fixities because we do not have
-- any fixity information to work with in the first place :(.
reparenTypePrec
  :: forall a
   . XRecCond a
  => Precedence
  -> HsType a
  -> HsType a
reparenTypePrec = go
  where
    -- Shorter name for 'reparenType'
    go :: Precedence -> HsType a -> HsType a
    go _ (HsBangTy x b ty) = HsBangTy x b (reparenLType ty)
    go _ (HsTupleTy x con tys) = HsTupleTy x con (map reparenLType tys)
    go _ (HsSumTy x tys) = HsSumTy x (map reparenLType tys)
    go _ (HsListTy x ty) = HsListTy x (reparenLType ty)
    go _ (HsRecTy x flds) = HsRecTy x (map (mapXRec @a reparenConDeclField) flds)
    go p (HsDocTy x ty d) = HsDocTy x (goL p ty) d
    go _ (HsExplicitListTy x p tys) = HsExplicitListTy x p (map reparenLType tys)
    go _ (HsExplicitTupleTy x tys) = HsExplicitTupleTy x (map reparenLType tys)
    go p (HsKindSig x ty kind) =
      paren p PREC_SIG $ HsKindSig x (goL PREC_SIG ty) (goL PREC_SIG kind)
    go p (HsIParamTy x n ty) =
      paren p PREC_SIG $ HsIParamTy x n (reparenLType ty)
    go p (HsForAllTy x tele ty) =
      paren p PREC_CTX $ HsForAllTy x (reparenHsForAllTelescope tele) (reparenLType ty)
    go p (HsQualTy x ctxt ty) =
      let p' [_] = PREC_CTX
          p' _ = PREC_TOP -- parens will get added anyways later...
          ctxt' = mapXRec @a (\xs -> map (goL (p' xs)) xs) ctxt
       in paren p PREC_CTX $ HsQualTy x ctxt' (goL PREC_TOP ty)
    go p (HsFunTy x w ty1 ty2) =
      paren p PREC_FUN $ HsFunTy x w (goL PREC_FUN ty1) (goL PREC_TOP ty2)
    go p (HsAppTy x fun_ty arg_ty) =
      paren p PREC_CON $ HsAppTy x (goL PREC_FUN fun_ty) (goL PREC_CON arg_ty)
    go p (HsAppKindTy x fun_ty arg_ki) =
      paren p PREC_CON $ HsAppKindTy x (goL PREC_FUN fun_ty) (goL PREC_CON arg_ki)
    go p (HsOpTy x prom ty1 op ty2) =
      paren p PREC_FUN $ HsOpTy x prom (goL PREC_OP ty1) op (goL PREC_OP ty2)
    go p (HsParTy _ t) = unXRec @a $ goL p t -- pretend the paren doesn't exist - it will be added back if needed
    go _ t@HsTyVar{} = t
    go _ t@HsStarTy{} = t
    go _ t@HsSpliceTy{} = t
    go _ t@HsTyLit{} = t
    go _ t@HsWildCardTy{} = t
    go _ t@XHsType{} = t

    -- Located variant of 'go'
    goL :: Precedence -> LHsType a -> LHsType a
    goL ctxt_prec = mapXRec @a (go ctxt_prec)

    -- Optionally wrap a type in parens
    paren
      :: Precedence -- Precedence of context
      -> Precedence -- Precedence of top-level operator
      -> HsType a
      -> HsType a -- Wrap in parens if (ctxt >= op)
    paren ctxt_prec op_prec
      | ctxt_prec >= op_prec = HsParTy noAnn . wrapXRec @a
      | otherwise = id

-- | Add parenthesis around the types in a 'HsType' (see 'reparenTypePrec')
reparenType :: XRecCond a => HsType a -> HsType a
reparenType = reparenTypePrec PREC_TOP

-- | Add parenthesis around the types in a 'LHsType' (see 'reparenTypePrec')
reparenLType :: forall a. XRecCond a => LHsType a -> LHsType a
reparenLType = mapXRec @a reparenType

-- | Add parentheses around the types in an 'HsSigType' (see 'reparenTypePrec')
reparenSigType
  :: forall a
   . XRecCond a
  => HsSigType a
  -> HsSigType a
reparenSigType (HsSig x bndrs body) =
  HsSig x (reparenOuterTyVarBndrs bndrs) (reparenLType body)
reparenSigType v@XHsSigType{} = v

-- | Add parentheses around the types in an 'HsOuterTyVarBndrs' (see 'reparenTypePrec')
reparenOuterTyVarBndrs
  :: forall flag a
   . XRecCond a
  => HsOuterTyVarBndrs flag a
  -> HsOuterTyVarBndrs flag a
reparenOuterTyVarBndrs imp@HsOuterImplicit{} = imp
reparenOuterTyVarBndrs (HsOuterExplicit x exp_bndrs) =
  HsOuterExplicit x (map (mapXRec @(NoGhcTc a) reparenTyVar) exp_bndrs)
reparenOuterTyVarBndrs v@XHsOuterTyVarBndrs{} = v

-- | Add parentheses around the types in an 'HsForAllTelescope' (see 'reparenTypePrec')
reparenHsForAllTelescope
  :: forall a
   . XRecCond a
  => HsForAllTelescope a
  -> HsForAllTelescope a
reparenHsForAllTelescope (HsForAllVis x bndrs) =
  HsForAllVis x (map (mapXRec @a reparenTyVar) bndrs)
reparenHsForAllTelescope (HsForAllInvis x bndrs) =
  HsForAllInvis x (map (mapXRec @a reparenTyVar) bndrs)
reparenHsForAllTelescope v@XHsForAllTelescope{} = v

-- | Add parenthesis around the types in a 'HsTyVarBndr' (see 'reparenTypePrec')
reparenTyVar :: XRecCond a => HsTyVarBndr flag a -> HsTyVarBndr flag a
reparenTyVar (UserTyVar x flag n) = UserTyVar x flag n
reparenTyVar (KindedTyVar x flag n kind) = KindedTyVar x flag n (reparenLType kind)
reparenTyVar v@XTyVarBndr{} = v

-- | Add parenthesis around the types in a 'ConDeclField' (see 'reparenTypePrec')
reparenConDeclField :: XRecCond a => ConDeclField a -> ConDeclField a
reparenConDeclField (ConDeclField x n t d) = ConDeclField x n (reparenLType t) d
reparenConDeclField c@XConDeclField{} = c

-------------------------------------------------------------------------------

-- * Located

-------------------------------------------------------------------------------

unL :: GenLocated l a -> a
unL (L _ x) = x

reL :: a -> GenLocated l a
reL = L undefined

mapMA :: Monad m => (a -> m b) -> LocatedAn an a -> m (Located b)
mapMA f (L al a) = L (locA al) <$> f a

-------------------------------------------------------------------------------

-- * NamedThing instances

-------------------------------------------------------------------------------

instance NamedThing (TyClDecl GhcRn) where
  getName = tcdName

-------------------------------------------------------------------------------

-- * Subordinates

-------------------------------------------------------------------------------

class Parent a where
  children :: a -> [Name]

instance Parent (ConDecl GhcRn) where
  children con =
    case getRecConArgs_maybe con of
      Nothing -> []
      Just flds -> map (foExt . unLoc) $ concatMap (cd_fld_names . unLoc) (unLoc flds)

instance Parent (TyClDecl GhcRn) where
  children d
    | isDataDecl d =
        map unLoc $
          concatMap (toList . getConNames . unLoc) $
            (dd_cons . tcdDataDefn) d
    | isClassDecl d =
        map (unLoc . fdLName . unLoc) (tcdATs d)
          ++ [unLoc n | L _ (TypeSig _ ns _) <- tcdSigs d, n <- ns]
    | otherwise = []

-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children

familyConDecl :: ConDecl GHC.GhcRn -> [(Name, [Name])]
familyConDecl d = zip (toList $ unLoc <$> getConNames d) (repeat $ children d)

-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl GhcRn -> [(Name, [Name])]
families d
  | isDataDecl d = family d : concatMap (familyConDecl . unLoc) (dd_cons (tcdDataDefn d))
  | isClassDecl d = [family d]
  | otherwise = []

-- | A mapping from child to parent
parentMap :: TyClDecl GhcRn -> [(Name, Name)]
parentMap d = [(c, p) | (p, cs) <- families d, c <- cs]

-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl GhcRn -> [Name]
parents n (TyClD _ d) = [p | (c, p) <- parentMap d, c == n]
parents _ _ = []

-------------------------------------------------------------------------------

-- * Utils that work in monads defined by GHC

-------------------------------------------------------------------------------

modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
modifySessionDynFlags f = do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (f dflags)
  return ()

-------------------------------------------------------------------------------

-- * DynFlags

-------------------------------------------------------------------------------

-- TODO: use `setOutputDir` from GHC
setOutputDir :: FilePath -> DynFlags -> DynFlags
setOutputDir dir dynFlags =
  dynFlags
    { objectDir = Just dir
    , hiDir = Just dir
    , hieDir = Just dir
    , stubDir = Just dir
    , includePaths = addGlobalInclude (includePaths dynFlags) [dir]
    , dumpDir = Just dir
    }

getSupportedLanguagesAndExtensions
  :: [Interface]
  -> [String]
getSupportedLanguagesAndExtensions [] = []
getSupportedLanguagesAndExtensions (iface : _) = do
  let dflags = ifaceDynFlags iface
   in supportedLanguagesAndExtensions dflags.targetPlatform.platformArchOS

-------------------------------------------------------------------------------

-- * 'StringBuffer' and 'ByteString'

-------------------------------------------------------------------------------
-- We get away with a bunch of these functions because 'StringBuffer' and
-- 'ByteString' have almost exactly the same structure.

-- | Convert a UTF-8 encoded 'ByteString' into a 'StringBuffer. This really
-- relies on the internals of both 'ByteString' and 'StringBuffer'.
--
-- /O(n)/ (but optimized into a @memcpy@ by @bytestring@ under the hood)
stringBufferFromByteString :: ByteString -> StringBuffer
stringBufferFromByteString bs =
  let BS.PS fp off len = bs <> BS.pack [0, 0, 0]
   in S.StringBuffer{S.buf = fp, S.len = len - 3, S.cur = off}

-- | Take the first @n@ /bytes/ of the 'StringBuffer' and put them in a
-- 'ByteString'.
--
-- /O(1)/
takeStringBuffer :: Int -> StringBuffer -> ByteString
takeStringBuffer !n (S.StringBuffer fp _ cur) = BS.PS fp cur n

-- | Return the prefix of the first 'StringBuffer' that /isn't/ in the second
-- 'StringBuffer'. **The behavior is undefined if the 'StringBuffers' use
-- separate buffers.**
--
-- /O(1)/
splitStringBuffer :: StringBuffer -> StringBuffer -> ByteString
splitStringBuffer buf1 buf2 = takeStringBuffer n buf1
  where
    n = S.byteDiff buf1 buf2

-- | Split the 'StringBuffer' at the next newline (or the end of the buffer).
-- Also: initial position is passed in and the updated position is returned.
--
-- /O(n)/ (but /O(1)/ space)
spanLine :: RealSrcLoc -> StringBuffer -> (ByteString, RealSrcLoc, StringBuffer)
spanLine !loc !buf = go loc buf
  where
    go !l !b
      | not (S.atEnd b) =
          case S.nextChar b of
            ('\n', b') -> (splitStringBuffer buf b', advanceSrcLoc l '\n', b')
            (c, b') -> go (advanceSrcLoc l c) b'
      | otherwise =
          (splitStringBuffer buf b, advanceSrcLoc l '\n', b)

-- | Given a start position and a buffer with that start position, split the
-- buffer at an end position.
--
-- /O(n)/ (but /O(1)/ space)
spanPosition
  :: RealSrcLoc
  -- ^ start of buffeer
  -> RealSrcLoc
  -- ^ position until which to take
  -> StringBuffer
  -- ^ buffer from which to take
  -> (ByteString, StringBuffer)
spanPosition !start !end !buf = go start buf
  where
    go !l !b
      | l < end
      , not (S.atEnd b)
      , (c, b') <- S.nextChar b =
          go (advanceSrcLoc l c) b'
      | otherwise =
          (splitStringBuffer buf b, b)

-- | Try to parse a line of CPP from the from of the buffer. A \"line\" of CPP
-- consists of
--
--   * at most 10 whitespace characters, including at least one newline
--   * a @#@ character
--   * keep parsing lines until you find a line not ending in @\\@.
--
-- This is chock full of heuristics about what a line of CPP is.
--
-- /O(n)/ (but /O(1)/ space)
tryCppLine :: RealSrcLoc -> StringBuffer -> Maybe (ByteString, RealSrcLoc, StringBuffer)
tryCppLine !loc !buf = spanSpace (S.prevChar buf '\n' == '\n') loc buf
  where
    -- Keep consuming space characters until we hit either a @#@ or something
    -- else. If we hit a @#@, start parsing CPP
    spanSpace !seenNl !l !b
      | S.atEnd b =
          Nothing
      | otherwise =
          case S.nextChar b of
            ('#', b')
              | not (S.atEnd b')
              , ('-', b'') <- S.nextChar b'
              , ('}', _) <- S.nextChar b'' ->
                  Nothing -- Edge case exception for @#-}@
              | seenNl ->
                  Just (spanCppLine (advanceSrcLoc l '#') b') -- parse CPP
              | otherwise ->
                  Nothing -- We didn't see a newline, so this can't be CPP!
            (c, b')
              | isSpace c ->
                  spanSpace
                    (seenNl || c == '\n')
                    (advanceSrcLoc l c)
                    b'
              | otherwise -> Nothing

    -- Consume a CPP line to its "end" (basically the first line that ends not
    -- with a @\@ character)
    spanCppLine !l !b
      | S.atEnd b =
          (splitStringBuffer buf b, l, b)
      | otherwise =
          case S.nextChar b of
            ('\\', b')
              | not (S.atEnd b')
              , ('\n', b'') <- S.nextChar b' ->
                  spanCppLine (advanceSrcLoc (advanceSrcLoc l '\\') '\n') b''
            ('\n', b') -> (splitStringBuffer buf b', advanceSrcLoc l '\n', b')
            (c, b') -> spanCppLine (advanceSrcLoc l c) b'

-------------------------------------------------------------------------------

-- * Names in a 'Type'

-------------------------------------------------------------------------------

-- | Given a 'Type', return a set of 'Name's coming from the 'TyCon's within
-- the type.
typeNames :: Type -> Set.Set Name
typeNames ty = go ty Set.empty
  where
    go :: Type -> Set.Set Name -> Set.Set Name
    go t acc =
      case t of
        TyVarTy{} -> acc
        AppTy t1 t2 -> go t2 $ go t1 acc
        FunTy _ _ t1 t2 -> go t2 $ go t1 acc
        TyConApp tcon args -> List.foldl' (\s t' -> go t' s) (Set.insert (getName tcon) acc) args
        ForAllTy bndr t' -> go t' $ go (tyVarKind (binderVar bndr)) acc
        LitTy _ -> acc
        CastTy t' _ -> go t' acc
        CoercionTy{} -> acc

-------------------------------------------------------------------------------

-- * Free variables of a 'Type'

-------------------------------------------------------------------------------

-- | Get free type variables in a 'Type' in their order of appearance.
-- See [Ordering of implicit variables].
orderedFVs
  :: VarSet
  -- ^ free variables to ignore
  -> [Type]
  -- ^ types to traverse (in order) looking for free variables
  -> [TyVar]
  -- ^ free type variables, in the order they appear in
orderedFVs vs tys =
  reverse . fst $ tyCoFVsOfTypes' tys (const True) vs ([], emptyVarSet)

-- See the "Free variables of types and coercions" section in 'TyCoRep', or
-- check out Note [Free variables of types]. The functions in this section
-- don't output type variables in the order they first appear in in the 'Type'.
--
-- For example, 'tyCoVarsOfTypeList' reports an incorrect order for the type
-- of 'const :: a -> b -> a':
--
-- >>> import GHC.Types.Name
-- >>> import TyCoRep
-- >>> import GHC.Builtin.Types.Prim
-- >>> import GHC.Types.Var
-- >>> a = TyVarTy alphaTyVar
-- >>> b = TyVarTy betaTyVar
-- >>> constTy = mkFunTys [a, b] a
-- >>> map (getOccString . tyVarName) (tyCoVarsOfTypeList constTy)
-- ["b","a"]
--
-- However, we want to reuse the very optimized traversal machinery there, so
-- so we make our own `tyCoFVsOfType'`, `tyCoFVsBndr'`, and `tyCoVarsOfTypes'`.
-- All these do differently is traverse in a different order and ignore
-- coercion variables.

-- | Just like 'tyCoFVsOfType', but traverses type variables in reverse order
-- of  appearance.
tyCoFVsOfType' :: Type -> FV
tyCoFVsOfType' (TyVarTy v) a b c = (FV.unitFV v `unionFV` tyCoFVsOfType' (tyVarKind v)) a b c
tyCoFVsOfType' (TyConApp _ tys) a b c = tyCoFVsOfTypes' tys a b c
tyCoFVsOfType' (LitTy{}) a b c = emptyFV a b c
tyCoFVsOfType' (AppTy fun arg) a b c = (tyCoFVsOfType' arg `unionFV` tyCoFVsOfType' fun) a b c
tyCoFVsOfType' (FunTy _ w arg res) a b c =
  ( tyCoFVsOfType' w
      `unionFV` tyCoFVsOfType' res
      `unionFV` tyCoFVsOfType' arg
  )
    a
    b
    c
tyCoFVsOfType' (ForAllTy bndr ty) a b c = tyCoFVsBndr' bndr (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CastTy ty _) a b c = (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CoercionTy _) a b c = emptyFV a b c

-- | Just like 'tyCoFVsOfTypes', but traverses type variables in reverse order
-- of appearance.
tyCoFVsOfTypes' :: [Type] -> FV
tyCoFVsOfTypes' (ty : tys) fv_cand in_scope acc = (tyCoFVsOfTypes' tys `unionFV` tyCoFVsOfType' ty) fv_cand in_scope acc
tyCoFVsOfTypes' [] fv_cand in_scope acc = emptyFV fv_cand in_scope acc

-- | Just like 'tyCoFVsBndr', but traverses type variables in reverse order of
-- appearance.
tyCoFVsBndr' :: TyVarBinder -> FV -> FV
tyCoFVsBndr' (Bndr tv _) fvs = FV.delFV tv fvs `unionFV` tyCoFVsOfType' (tyVarKind tv)

-------------------------------------------------------------------------------

-- * Defaulting RuntimeRep variables

-------------------------------------------------------------------------------

-- | Traverses the type, defaulting type variables of kind 'RuntimeRep' to
-- 'LiftedType'. See 'defaultRuntimeRepVars' in GHC.Iface.Type the original such
-- function working over `IfaceType`'s.
defaultRuntimeRepVars :: Type -> Type
defaultRuntimeRepVars = go emptyVarEnv
  where
    go :: TyVarEnv () -> Type -> Type
    go subs (ForAllTy (Bndr var flg) ty)
      | isRuntimeRepVar var
      , isInvisibleForAllTyFlag flg =
          let subs' = extendVarEnv subs var ()
           in go subs' ty
      | otherwise =
          ForAllTy
            (Bndr (updateTyVarKind (go subs) var) flg)
            (go subs ty)
    go subs (TyVarTy tv)
      | tv `elemVarEnv` subs =
          liftedRepTy
      | otherwise =
          TyVarTy (updateTyVarKind (go subs) tv)
    go subs (TyConApp tc tc_args) =
      TyConApp tc (map (go subs) tc_args)
    go subs (FunTy af w arg res) =
      FunTy af (go subs w) (go subs arg) (go subs res)
    go subs (AppTy t u) =
      AppTy (go subs t) (go subs u)
    go subs (CastTy x co) =
      CastTy (go subs x) co
    go _ ty@(LitTy{}) = ty
    go _ ty@(CoercionTy{}) = ty

fromMaybeContext :: Maybe (LHsContext DocNameI) -> HsContext DocNameI
fromMaybeContext mctxt = unLoc $ fromMaybe (noLocA []) mctxt

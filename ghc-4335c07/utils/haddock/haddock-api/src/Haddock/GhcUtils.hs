{-# LANGUAGE BangPatterns, FlexibleInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
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
-----------------------------------------------------------------------------
module Haddock.GhcUtils where


import Control.Arrow
import Haddock.Types( DocNameI )

import Exception
import Outputable
import Name
import NameSet
import Lexeme
import Module
import HscTypes
import GHC
import Class


moduleString :: Module -> String
moduleString = moduleNameString . moduleName

isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName


isVarSym :: OccName -> Bool
isVarSym = isLexVarSym . occNameFS

isConSym :: OccName -> Bool
isConSym = isLexConSym . occNameFS


getMainDeclBinder :: HsDecl name -> [IdP name]
getMainDeclBinder (TyClD d) = [tcdName d]
getMainDeclBinder (ValD d) =
  case collectHsBindBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder (SigD d) = sigNameNoLoc d
getMainDeclBinder (ForD (ForeignImport name _ _ _)) = [unLoc name]
getMainDeclBinder (ForD (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ = []

-- Extract the source location where an instance is defined. This is used
-- to correlate InstDecls with their Instance/CoAxiom Names, via the
-- instanceMap.
getInstLoc :: InstDecl name -> SrcSpan
getInstLoc (ClsInstD (ClsInstDecl { cid_poly_ty = ty })) = getLoc (hsSigType ty)
getInstLoc (DataFamInstD (DataFamInstDecl
  { dfid_eqn = HsIB { hsib_body = FamEqn { feqn_tycon = L l _ }}})) = l
getInstLoc (TyFamInstD (TyFamInstDecl
  -- Since CoAxioms' Names refer to the whole line for type family instances
  -- in particular, we need to dig a bit deeper to pull out the entire
  -- equation. This does not happen for data family instances, for some reason.
  { tfid_eqn = HsIB { hsib_body = FamEqn { feqn_rhs = L l _ }}})) = l

-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (IdP name -> Bool) -> LSig name -> Maybe (LSig name)
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (IdP name -> Bool) -> Sig name -> Maybe (Sig name)
filterSigNames p orig@(SpecSig n _ _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig n _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p (FixSig (FixitySig ns ty)) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (FixSig (FixitySig filtered ty))
filterSigNames _ orig@(MinimalSig _ _)      = Just orig
filterSigNames p (TypeSig ns ty) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (TypeSig filtered ty)
filterSigNames p (ClassOpSig is_default ns ty) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (ClassOpSig is_default filtered ty)
filterSigNames p (PatSynSig ns ty) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (PatSynSig filtered ty)
filterSigNames _ _                           = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True  = Just
ifTrueJust False = const Nothing

sigName :: LSig name -> [IdP name]
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> [IdP name]
sigNameNoLoc (TypeSig      ns _)       = map unLoc ns
sigNameNoLoc (ClassOpSig _ ns _)       = map unLoc ns
sigNameNoLoc (PatSynSig    ns _)       = map unLoc ns
sigNameNoLoc (SpecSig      n _ _)      = [unLoc n]
sigNameNoLoc (InlineSig    n _)        = [unLoc n]
sigNameNoLoc (FixSig (FixitySig ns _)) = map unLoc ns
sigNameNoLoc _                         = []

-- | Was this signature given by the user?
isUserLSig :: LSig name -> Bool
isUserLSig (L _(TypeSig {}))    = True
isUserLSig (L _(ClassOpSig {})) = True
isUserLSig (L _(PatSynSig {}))  = True
isUserLSig _                    = False


isClassD :: HsDecl a -> Bool
isClassD (TyClD d) = isClassDecl d
isClassD _ = False

isValD :: HsDecl a -> Bool
isValD (ValD _) = True
isValD _ = False


declATs :: HsDecl a -> [IdP a]
declATs (TyClD d) | isClassDecl d = map (unL . fdLName . unL) $ tcdATs d
declATs _ = []


pretty :: Outputable a => DynFlags -> a -> String
pretty = showPpr

nubByName :: (a -> Name) -> [a] -> [a]
nubByName f ns = go emptyNameSet ns
  where
    go !_ [] = []
    go !s (x:xs)
      | y `elemNameSet` s = go s xs
      | otherwise         = let !s' = extendNameSet s y
                            in x : go s' xs
      where
        y = f x

getGADTConType :: ConDecl p -> LHsType p
-- The full type of a GADT data constructor We really only get this in
-- order to pretty-print it, and currently only in Haddock's code.  So
-- we are cavalier about locations and extensions, hence the
-- 'undefined's
getGADTConType (ConDeclGADT { con_forall = has_forall
                            , con_qvars = qtvs
                            , con_mb_cxt = mcxt, con_args = args
                            , con_res_ty = res_ty })
 | has_forall = noLoc (HsForAllTy { hst_bndrs = hsQTvExplicit qtvs
                                  , hst_body  = theta_ty })
 | otherwise  = theta_ty
 where
   theta_ty | Just theta <- mcxt
            = noLoc (HsQualTy { hst_ctxt = theta, hst_body = tau_ty })
            | otherwise
            = tau_ty

   tau_ty = case args of
              RecCon flds -> noLoc (HsFunTy (noLoc (HsRecTy (unLoc flds))) res_ty)
              PrefixCon pos_args -> foldr (\ a b -> noLoc (HsFunTy a b)) res_ty pos_args
              InfixCon {} -> panic "InfixCon for GADT"

getGADTConType (ConDeclH98 {}) = panic "getGADTConType"
  -- Should only be called on ConDeclGADT

-------------------------------------------------------------------------------
-- * Located
-------------------------------------------------------------------------------


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined

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
    case con_args con of
      RecCon fields -> map (selectorFieldOcc . unL) $
                         concatMap (cd_fld_names . unL) (unL fields)
      _             -> []

instance Parent (TyClDecl GhcRn) where
  children d
    | isDataDecl  d = map unL $ concatMap (getConNames . unL)
                              $ (dd_cons . tcdDataDefn) $ d
    | isClassDecl d =
        map (unL . fdLName . unL) (tcdATs d) ++
        [ unL n | L _ (TypeSig ns _) <- tcdSigs d, n <- ns ]
    | otherwise = []


-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children


familyConDecl :: ConDecl GHC.GhcRn -> [(Name, [Name])]
familyConDecl d = zip (map unL (getConNames d)) (repeat $ children d)

-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl GhcRn -> [(Name, [Name])]
families d
  | isDataDecl  d = family d : concatMap (familyConDecl . unL) (dd_cons (tcdDataDefn d))
  | isClassDecl d = [family d]
  | otherwise     = []


-- | A mapping from child to parent
parentMap :: TyClDecl GhcRn -> [(Name, Name)]
parentMap d = [ (c, p) | (p, cs) <- families d, c <- cs ]


-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl GhcRn -> [Name]
parents n (TyClD d) = [ p | (c, p) <- parentMap d, c == n ]
parents _ _ = []


-------------------------------------------------------------------------------
-- * Utils that work in monads defined by GHC
-------------------------------------------------------------------------------


modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
modifySessionDynFlags f = do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (f dflags)
  return ()


-- | A variant of 'gbracket' where the return value from the first computation
-- is not required.
gbracket_ :: ExceptionMonad m => m a -> m b -> m c -> m c
gbracket_ before_ after thing = gbracket before_ (const after) (const thing)

-- Extract the minimal complete definition of a Name, if one exists
minimalDef :: GhcMonad m => Name -> m (Maybe ClassMinimalDef)
minimalDef n = do
  mty <- lookupGlobalName n
  case mty of
    Just (ATyCon (tyConClass_maybe -> Just c)) -> return . Just $ classMinimalDef c
    _ -> return Nothing

-------------------------------------------------------------------------------
-- * DynFlags
-------------------------------------------------------------------------------


setObjectDir, setHiDir, setStubDir, setOutputDir :: String -> DynFlags -> DynFlags
setObjectDir  f d = d{ objectDir  = Just f}
setHiDir      f d = d{ hiDir      = Just f}
setStubDir    f d = d{ stubDir    = Just f, includePaths = f : includePaths d }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling with -fvia-C.
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f



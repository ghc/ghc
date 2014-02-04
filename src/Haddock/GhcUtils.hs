{-# LANGUAGE FlexibleInstances #-}
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


import Data.Version
import Control.Applicative  ( (<$>) )
import Control.Arrow
import Data.Foldable hiding (concatMap)
import Data.Function
import Data.Traversable
import Distribution.Compat.ReadP
import Distribution.Text

import Exception
import Outputable
import Name
import Packages
import Module
import RdrName (GlobalRdrEnv)
import GhcMonad (withSession)
import HscTypes
import UniqFM
import GHC


moduleString :: Module -> String
moduleString = moduleNameString . moduleName


-- return the (name,version) of the package
modulePackageInfo :: Module -> (String, [Char])
modulePackageInfo modu = case unpackPackageId pkg of
                          Nothing -> (packageIdString pkg, "")
                          Just x -> (display $ pkgName x, showVersion (pkgVersion x))
  where pkg = modulePackageId modu


-- This was removed from GHC 6.11
-- XXX we shouldn't be using it, probably

-- | Try and interpret a GHC 'PackageId' as a cabal 'PackageIdentifer'. Returns @Nothing@ if
-- we could not parse it as such an object.
unpackPackageId :: PackageId -> Maybe PackageIdentifier
unpackPackageId p
  = case [ pid | (pid,"") <- readP_to_S parse str ] of
        []      -> Nothing
        (pid:_) -> Just pid
  where str = packageIdString p


lookupLoadedHomeModuleGRE  :: GhcMonad m => ModuleName -> m (Maybe GlobalRdrEnv)
lookupLoadedHomeModuleGRE mod_name = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (mi_globals (hm_iface mod_info))
    _not_a_home_module -> return Nothing


isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName


isVarSym :: OccName -> Bool
isVarSym = isLexVarSym . occNameFS

isConSym :: OccName -> Bool
isConSym = isLexConSym . occNameFS


getMainDeclBinder :: HsDecl name -> [name]
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
getInstLoc (ClsInstD (ClsInstDecl { cid_poly_ty = L l _ })) = l
getInstLoc (DataFamInstD (DataFamInstDecl { dfid_tycon = L l _ })) = l
getInstLoc (TyFamInstD (TyFamInstDecl
  -- Since CoAxioms' Names refer to the whole line for type family instances
  -- in particular, we need to dig a bit deeper to pull out the entire
  -- equation. This does not happen for data family instances, for some reason.
  { tfid_eqn = L _ (TyFamInstEqn { tfie_rhs = L l _ })})) = l

-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (name -> Bool) -> LSig name -> Maybe (LSig name)
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (name -> Bool) -> Sig name -> Maybe (Sig name)
filterSigNames p orig@(SpecSig n _ _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig n _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(FixSig (FixitySig n _)) = ifTrueJust (p $ unLoc n) orig
filterSigNames p (TypeSig ns ty)               =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (TypeSig filtered ty)
filterSigNames _ _                           = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True  = Just
ifTrueJust False = const Nothing

sigName :: LSig name -> [name]
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> [name]
sigNameNoLoc (TypeSig   ns _)         = map unLoc ns
sigNameNoLoc (PatSynSig n _ _ _ _)    = [unLoc n]
sigNameNoLoc (SpecSig   n _ _)        = [unLoc n]
sigNameNoLoc (InlineSig n _)          = [unLoc n]
sigNameNoLoc (FixSig (FixitySig n _)) = [unLoc n]
sigNameNoLoc _                        = []


isTyClD :: HsDecl a -> Bool
isTyClD (TyClD _) = True
isTyClD _ = False


isClassD :: HsDecl a -> Bool
isClassD (TyClD d) = isClassDecl d
isClassD _ = False


isDocD :: HsDecl a -> Bool
isDocD (DocD _) = True
isDocD _ = False


isInstD :: HsDecl a -> Bool
isInstD (InstD _) = True
isInstD _ = False


isValD :: HsDecl a -> Bool
isValD (ValD _) = True
isValD _ = False


declATs :: HsDecl a -> [a]
declATs (TyClD d) | isClassDecl d = map (unL . fdLName . unL) $ tcdATs d
declATs _ = []


pretty :: Outputable a => DynFlags -> a -> String
pretty = showPpr


trace_ppr :: Outputable a => DynFlags -> a -> b -> b
trace_ppr dflags x y = trace (pretty dflags x) y


-------------------------------------------------------------------------------
-- * Located
-------------------------------------------------------------------------------


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined


before :: Located a -> Located a -> Bool
before = (<) `on` getLoc


instance Foldable (GenLocated l) where
  foldMap f (L _ x) = f x


instance Traversable (GenLocated l) where
  mapM f (L l x) = (return . L l) =<< f x
  traverse f (L l x) = L l <$> f x

-------------------------------------------------------------------------------
-- * NamedThing instances
-------------------------------------------------------------------------------


instance NamedThing (TyClDecl Name) where
  getName = tcdName


instance NamedThing (ConDecl Name) where
  getName = unL . con_name


-------------------------------------------------------------------------------
-- * Subordinates
-------------------------------------------------------------------------------


class Parent a where
  children :: a -> [Name]


instance Parent (ConDecl Name) where
  children con =
    case con_details con of
      RecCon fields -> map (unL . cd_fld_name) fields
      _             -> []


instance Parent (TyClDecl Name) where
  children d
    | isDataDecl  d = map (unL . con_name . unL) . dd_cons . tcdDataDefn $ d
    | isClassDecl d =
        map (unL . fdLName . unL) (tcdATs d) ++
        [ unL n | L _ (TypeSig ns _) <- tcdSigs d, n <- ns ]
    | otherwise = []


-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children


-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl Name -> [(Name, [Name])]
families d
  | isDataDecl  d = family d : map (family . unL) (dd_cons (tcdDataDefn d))
  | isClassDecl d = [family d]
  | otherwise     = []


-- | A mapping from child to parent
parentMap :: TyClDecl Name -> [(Name, Name)]
parentMap d = [ (c, p) | (p, cs) <- families d, c <- cs ]


-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl Name -> [Name]
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


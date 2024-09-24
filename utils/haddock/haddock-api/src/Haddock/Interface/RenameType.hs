{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Haddock.Interface.RenameType
  ( rename
  ) where

import Control.Monad.Trans.State
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC
import GHC.Data.FastString
import GHC.Types.Name

import Haddock.Types

-- | Haskell AST type representation.
--
-- This type is used for renaming (more below), essentially the ambiguous (!)
-- version of 'Name'. So, why is this 'FastString' instead of 'OccName'? Well,
-- it was 'OccName' before, but turned out that 'OccName' sometimes also
-- contains namespace information, differentiating visually same types.
--
-- And 'FastString' is used because it is /visual/ part of 'OccName' - it is
-- not converted to 'String' or alike to avoid new allocations. Additionally,
-- since it is stored mostly in 'Set', fast comparison of 'FastString' is also
-- quite nice.
newtype NameRep
  = NameRep FastString
  deriving (Eq)

instance Ord NameRep where
  compare (NameRep fs1) (NameRep fs2) = uniqCompareFS fs1 fs2

getNameRep :: NamedThing name => name -> NameRep
getNameRep = NameRep . getOccFS

nameRepString :: NameRep -> String
nameRepString (NameRep fs) = unpackFS fs

stringNameRep :: String -> NameRep
stringNameRep = NameRep . mkFastString

setInternalNameRep :: SetName name => NameRep -> name -> name
setInternalNameRep (NameRep fs) = setInternalOccName (mkVarOccFS fs)

setInternalOccName :: SetName name => OccName -> name -> name
setInternalOccName occ name =
  setName nname' name
  where
    nname = getName name
    nname' = mkInternalName (nameUnique nname) occ (nameSrcSpan nname)

-- | Make given type visually unambiguous.
--
-- After applying 'specialize' method, some free type variables may become
-- visually ambiguous - for example, having @a -> b@ and specializing @a@ to
-- @(a -> b)@ we get @(a -> b) -> b@ where first occurrence of @b@ refers to
-- different type variable than latter one. Applying 'rename' function
-- will fix that type to be visually unambiguous again (making it something
-- like @(a -> b0) -> b@).
rename :: [Name] -> LHsType GhcRn -> LHsType GhcRn
rename [] typ = typ
rename fv typ = evalState (traverse renameType typ) env
  where
    env =
      RenameEnv
        { rneHeadFVs = Map.fromList $ map mkPair fv
        , rneCtx = Map.empty
        }
    mkPair name = (getNameRep name, name)

-- | Renaming monad.
type Rename name = State (RenameEnv name)

data RenameEnv name = RenameEnv
  { rneHeadFVs :: Map NameRep Name
  , rneCtx :: Map Name name
  }

renameType :: HsType GhcRn -> Rename (IdP GhcRn) (HsType GhcRn)
renameType (HsForAllTy x tele lt) =
  HsForAllTy x
    <$> renameForAllTelescope tele
    <*> renameLType lt
renameType (HsQualTy x lctxt lt) =
  HsQualTy x
    <$> renameLContext lctxt
    <*> renameLType lt
renameType (HsTyVar x ip name) = HsTyVar x ip <$> locatedN renameName name
renameType t@(HsStarTy _ _) = pure t
renameType (HsAppTy x lf la) = HsAppTy x <$> renameLType lf <*> renameLType la
renameType (HsAppKindTy x lt lk) = HsAppKindTy x <$> renameLType lt <*> renameLKind lk
renameType (HsFunTy x w la lr) = HsFunTy x <$> renameHsArrow w <*> renameLType la <*> renameLType lr
renameType (HsListTy x lt) = HsListTy x <$> renameLType lt
renameType (HsTupleTy x srt lt) = HsTupleTy x srt <$> mapM renameLType lt
renameType (HsSumTy x lt) = HsSumTy x <$> mapM renameLType lt
renameType (HsOpTy x f la lop lb) =
  HsOpTy x <$> pure f <*> renameLType la <*> locatedN renameName lop <*> renameLType lb
renameType (HsParTy x lt) = HsParTy x <$> renameLType lt
renameType (HsIParamTy x ip lt) = HsIParamTy x ip <$> renameLType lt
renameType (HsKindSig x lt lk) = HsKindSig x <$> renameLType lt <*> pure lk
renameType t@(HsSpliceTy _ _) = pure t
renameType (HsDocTy x lt doc) = HsDocTy x <$> renameLType lt <*> pure doc
renameType (HsBangTy x bang lt) = HsBangTy x bang <$> renameLType lt
renameType t@(HsRecTy _ _) = pure t
renameType t@(XHsType _) = pure t
renameType (HsExplicitListTy x ip ltys) =
  HsExplicitListTy x ip <$> renameLTypes ltys
renameType (HsExplicitTupleTy x ltys) =
  HsExplicitTupleTy x <$> renameLTypes ltys
renameType t@(HsTyLit _ _) = pure t
renameType (HsWildCardTy wc) = pure (HsWildCardTy wc)

renameModifier :: HsModifier GhcRn -> Rename (IdP GhcRn) (HsModifier GhcRn)
renameModifier (HsModifier x ty) = HsModifier x <$> renameLType ty

renameModifiers :: [HsModifier GhcRn] -> Rename (IdP GhcRn) [HsModifier GhcRn]
renameModifiers = mapM renameModifier

renameHsArrow :: HsArrow GhcRn -> Rename (IdP GhcRn) (HsArrow GhcRn)
renameHsArrow arr@(HsUnrestrictedArrow _) = pure arr
renameHsArrow (HsLinearArrow x p) = HsLinearArrow x <$> renameModifiers p
renameHsArrow (HsExplicitMult x p) = HsExplicitMult x <$> renameModifiers p

renameLType :: LHsType GhcRn -> Rename (IdP GhcRn) (LHsType GhcRn)
renameLType = located renameType

renameLKind :: LHsKind GhcRn -> Rename (IdP GhcRn) (LHsKind GhcRn)
renameLKind = renameLType

renameLTypes :: [LHsType GhcRn] -> Rename (IdP GhcRn) [LHsType GhcRn]
renameLTypes = mapM renameLType

renameLContext :: LHsContext GhcRn -> Rename (IdP GhcRn) (LHsContext GhcRn)
renameLContext (L l ctxt) = do
  ctxt' <- renameContext ctxt
  return (L l ctxt')

renameContext :: HsContext GhcRn -> Rename (IdP GhcRn) (HsContext GhcRn)
renameContext = renameLTypes

renameForAllTelescope
  :: HsForAllTelescope GhcRn
  -> Rename (IdP GhcRn) (HsForAllTelescope GhcRn)
renameForAllTelescope (HsForAllVis x bndrs) =
  HsForAllVis x <$> mapM renameLBinder bndrs
renameForAllTelescope (HsForAllInvis x bndrs) =
  HsForAllInvis x <$> mapM renameLBinder bndrs

renameBinder :: HsTyVarBndr flag GhcRn -> Rename (IdP GhcRn) (HsTyVarBndr flag GhcRn)
renameBinder (HsTvb x fl bvar lkind) =
  HsTvb x fl <$> renameBinderVar bvar <*> renameBinderKind lkind

renameBinderVar :: HsBndrVar GhcRn -> Rename (IdP GhcRn) (HsBndrVar GhcRn)
renameBinderVar (HsBndrVar x lname) = HsBndrVar x <$> locatedN renameName lname
renameBinderVar (HsBndrWildCard x) = return (HsBndrWildCard x)

renameBinderKind :: HsBndrKind GhcRn -> Rename (IdP GhcRn) (HsBndrKind GhcRn)
renameBinderKind (HsBndrNoKind x) = return (HsBndrNoKind x)
renameBinderKind (HsBndrKind x k) = HsBndrKind x <$> located renameType k

renameLBinder :: LHsTyVarBndr flag GhcRn -> Rename (IdP GhcRn) (LHsTyVarBndr flag GhcRn)
renameLBinder = located renameBinder

-- | Core renaming logic.
renameName :: SetName name => name -> Rename name name
renameName name = do
  RenameEnv{..} <- get
  case Map.lookup (getName name) rneCtx of
    Nothing
      | Just headTv <- Map.lookup (getNameRep name) rneHeadFVs
      , headTv /= getName name ->
          freshName name
    Just name' -> return name'
    _ -> return name

-- | Generate fresh occurrence name, put it into context and return.
freshName :: SetName name => name -> Rename name name
freshName name = do
  taken <- takenNames
  let name' = setInternalNameRep (findFreshName taken rep) name
  modify $ \rne ->
    rne
      { rneCtx = Map.insert (getName name) name' (rneCtx rne)
      }
  return name'
  where
    nname = getName name
    rep = getNameRep nname

takenNames :: NamedThing name => Rename name (Set NameRep)
takenNames = do
  RenameEnv{..} <- get
  return $ Set.unions [headReps rneHeadFVs, ctxElems rneCtx]
  where
    headReps = Set.fromList . Map.keys
    ctxElems = Set.fromList . map getNameRep . Map.elems

findFreshName :: Set NameRep -> NameRep -> NameRep
findFreshName taken =
  fromJust . List.find isFresh . alternativeNames
  where
    isFresh = not . flip Set.member taken

alternativeNames :: NameRep -> [NameRep]
alternativeNames name =
  [stringNameRep $ str ++ show i | i :: Int <- [0 ..]]
  where
    str = nameRepString name

located :: Functor f => (a -> f b) -> GenLocated l a -> f (GenLocated l b)
located f (L loc e) = L loc <$> f e

locatedN :: Functor f => (a -> f b) -> LocatedN a -> f (LocatedN b)
locatedN f (L loc e) = L loc <$> f e

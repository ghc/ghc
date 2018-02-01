{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Haddock.Interface.Specialize
    ( specializeInstHead
    ) where


import Haddock.Syb
import Haddock.Types

import GHC
import Name
import FastString

import Control.Monad
import Control.Monad.Trans.State

import Data.Data
import qualified Data.List as List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Instantiate all occurrences of given names with corresponding types.
specialize :: forall name a. (Ord (IdP name), DataId name, NamedThing (IdP name))
            => Data a
            => [(IdP name, HsType name)] -> a -> a
specialize specs = go spec_map0
  where
    go :: forall x. Data x => Map (IdP name) (HsType name) -> x -> x
    go spec_map = everywhereButType @name $ mkT $ sugar . strip_kind_sig . specialize_ty_var spec_map

    strip_kind_sig :: HsType name -> HsType name
    strip_kind_sig (HsKindSig (L _ t) _) = t
    strip_kind_sig typ = typ

    specialize_ty_var :: Map (IdP name) (HsType name) -> HsType name -> HsType name
    specialize_ty_var spec_map (HsTyVar _ (L _ name'))
      | Just t <- Map.lookup name' spec_map = t
    specialize_ty_var _ typ = typ

    -- This is a tricky recursive definition. By adding in the specializations
    -- one by one, we should avoid infinite loops.
    spec_map0 = foldr (\(n,t) acc -> Map.insert n (go acc t) acc) mempty specs


-- | Instantiate given binders with corresponding types.
--
-- Again, it is just a convenience function around 'specialize'. Note that
-- length of type list should be the same as the number of binders.
specializeTyVarBndrs :: (Ord (IdP name), DataId name, NamedThing (IdP name))
                     => Data a
                     => LHsQTyVars name -> [HsType name]
                     -> a -> a
specializeTyVarBndrs bndrs typs =
    specialize $ zip bndrs' typs
  where
    bndrs' = map (bname . unLoc) . hsq_explicit $ bndrs
    bname (UserTyVar (L _ name)) = name
    bname (KindedTyVar (L _ name) _) = name


specializePseudoFamilyDecl :: (Ord (IdP name), DataId name, NamedThing (IdP name))
                           => LHsQTyVars name -> [HsType name]
                           -> PseudoFamilyDecl name
                           -> PseudoFamilyDecl name
specializePseudoFamilyDecl bndrs typs decl =
  decl {pfdTyVars = map (specializeTyVarBndrs bndrs typs) (pfdTyVars decl)}

specializeSig :: forall name . (Ord (IdP name), DataId name, SetName (IdP name), NamedThing (IdP name))
              => LHsQTyVars name -> [HsType name]
              -> Sig name
              -> Sig name
specializeSig bndrs typs (TypeSig lnames typ) =
  TypeSig lnames (typ {hswc_body = (hswc_body typ) {hsib_body = noLoc typ'}})
  where
    true_type :: HsType name
    true_type = unLoc (hsSigWcType typ)
    typ' :: HsType name
    typ' = rename fv $ specializeTyVarBndrs bndrs typs true_type
    fv = foldr Set.union Set.empty . map freeVariables $ typs
specializeSig _ _ sig = sig


-- | Make all details of instance head (signatures, associated types)
-- specialized to that particular instance type.
specializeInstHead :: (Ord (IdP name), DataId name, SetName (IdP name), NamedThing (IdP name))
                   => InstHead name -> InstHead name
specializeInstHead ihd@InstHead { ihdInstType = clsi@ClassInst { .. }, .. } =
    ihd { ihdInstType = instType' }
  where
    instType' = clsi
        { clsiSigs = map specializeSig' clsiSigs
        , clsiAssocTys = map specializeFamilyDecl' clsiAssocTys
        }
    specializeSig' = specializeSig clsiTyVars ihdTypes
    specializeFamilyDecl' = specializePseudoFamilyDecl clsiTyVars ihdTypes
specializeInstHead ihd = ihd


-- | Make given type use tuple and list literals where appropriate.
--
-- After applying 'specialize' function some terms may not use idiomatic list
-- and tuple literals resulting in types like @[] a@ or @(,,) a b c@. This
-- can be fixed using 'sugar' function, that will turn such types into @[a]@
-- and @(a, b, c)@.
sugar :: forall name. (NamedThing (IdP name), DataId name)
      => HsType name -> HsType name
sugar = sugarOperators . sugarTuples . sugarLists

sugarLists :: NamedThing (IdP name) => HsType name -> HsType name
sugarLists (HsAppTy (L _ (HsTyVar _ (L _ name))) ltyp)
    | isBuiltInSyntax name' && strName == "[]" = HsListTy ltyp
  where
    name' = getName name
    strName = occNameString . nameOccName $ name'
sugarLists typ = typ


sugarTuples :: NamedThing (IdP name) => HsType name -> HsType name
sugarTuples typ =
    aux [] typ
  where
    aux apps (HsAppTy (L _ ftyp) atyp) = aux (atyp:apps) ftyp
    aux apps (HsParTy (L _ typ')) = aux apps typ'
    aux apps (HsTyVar _ (L _ name))
        | isBuiltInSyntax name' && suitable = HsTupleTy HsBoxedTuple apps
      where
        name' = getName name
        strName = occNameString . nameOccName $ name'
        suitable = case parseTupleArity strName of
            Just arity -> arity == length apps
            Nothing -> False
    aux _ _ = typ


sugarOperators :: NamedThing (IdP name) => HsType name -> HsType name
sugarOperators (HsAppTy (L _ (HsAppTy (L _ (HsTyVar _ (L l name))) la)) lb)
    | isSymOcc $ getOccName name' = mkHsOpTy la (L l name) lb
    | isBuiltInSyntax name' && getOccString name == "(->)" = HsFunTy la lb
  where
    name' = getName name
sugarOperators typ = typ


-- | Compute arity of given tuple operator.
--
-- >>> parseTupleArity "(,,)"
-- Just 3
--
-- >>> parseTupleArity "(,,,,)"
-- Just 5
--
-- >>> parseTupleArity "abc"
-- Nothing
--
-- >>> parseTupleArity "()"
-- Nothing
parseTupleArity :: String -> Maybe Int
parseTupleArity ('(':commas) = do
    n <- parseCommas commas
    guard $ n /= 0
    return $ n + 1
  where
    parseCommas (',':rest) = (+ 1) <$> parseCommas rest
    parseCommas ")" = Just 0
    parseCommas _ = Nothing
parseTupleArity _ = Nothing


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
type NameRep = FastString

getNameRep :: NamedThing name => name -> NameRep
getNameRep = occNameFS . getOccName

nameRepString :: NameRep -> String
nameRepString = unpackFS

stringNameRep :: String -> NameRep
stringNameRep = mkFastString

setInternalNameRep :: SetName name => NameRep -> name -> name
setInternalNameRep = setInternalOccName . mkVarOccFS

setInternalOccName :: SetName name => OccName -> name -> name
setInternalOccName occ name =
    setName nname' name
  where
    nname = getName name
    nname' = mkInternalName (nameUnique nname) occ (nameSrcSpan nname)


-- | Compute set of free variables of given type.
freeVariables :: forall name. (NamedThing (IdP name), DataId name)
              => HsType name -> Set Name
freeVariables =
    everythingWithState Set.empty Set.union query
  where
    query term ctx = case cast term :: Maybe (HsType name) of
        Just (HsForAllTy bndrs _) ->
            (Set.empty, Set.union ctx (bndrsNames bndrs))
        Just (HsTyVar _ (L _ name))
            | getName name `Set.member` ctx -> (Set.empty, ctx)
            | otherwise -> (Set.singleton $ getName name, ctx)
        _ -> (Set.empty, ctx)
    bndrsNames = Set.fromList . map (getName . tyVarName . unLoc)


-- | Make given type visually unambiguous.
--
-- After applying 'specialize' method, some free type variables may become
-- visually ambiguous - for example, having @a -> b@ and specializing @a@ to
-- @(a -> b)@ we get @(a -> b) -> b@ where first occurrence of @b@ refers to
-- different type variable than latter one. Applying 'rename' function
-- will fix that type to be visually unambiguous again (making it something
-- like @(a -> b0) -> b@).
rename :: (Eq (IdP name), DataId name, SetName (IdP name))
       => Set Name-> HsType name -> HsType name
rename fv typ = evalState (renameType typ) env
  where
    env = RenameEnv
      { rneHeadFVs = Map.fromList . map mkPair . Set.toList $ fv
      , rneSigFVs = Set.map getNameRep $ freeVariables typ
      , rneCtx = Map.empty
      }
    mkPair name = (getNameRep name, name)

-- | Renaming monad.
type Rename name = State (RenameEnv name)

data RenameEnv name = RenameEnv
  { rneHeadFVs :: Map NameRep Name
  , rneSigFVs :: Set NameRep
  , rneCtx :: Map Name name
  }


renameType :: (Eq (IdP name), SetName (IdP name))
           => HsType name -> Rename (IdP name) (HsType name)
renameType (HsForAllTy bndrs lt) =
    HsForAllTy
        <$> mapM (located renameBinder) bndrs
        <*> renameLType lt
renameType (HsQualTy lctxt lt) =
    HsQualTy
        <$> located renameContext lctxt
        <*> renameLType lt
renameType (HsTyVar ip name) = HsTyVar ip <$> located renameName name
renameType (HsAppTy lf la) = HsAppTy <$> renameLType lf <*> renameLType la
renameType (HsFunTy la lr) = HsFunTy <$> renameLType la <*> renameLType lr
renameType (HsListTy lt) = HsListTy <$> renameLType lt
renameType (HsPArrTy lt) = HsPArrTy <$> renameLType lt
renameType (HsTupleTy srt lt) = HsTupleTy srt <$> mapM renameLType lt
renameType (HsSumTy lt) = HsSumTy <$> mapM renameLType lt
renameType (HsOpTy la lop lb) =
    HsOpTy <$> renameLType la <*> located renameName lop <*> renameLType lb
renameType (HsParTy lt) = HsParTy <$> renameLType lt
renameType (HsIParamTy ip lt) = HsIParamTy ip <$> renameLType lt
renameType (HsEqTy la lb) = HsEqTy <$> renameLType la <*> renameLType lb
renameType (HsKindSig lt lk) = HsKindSig <$> renameLType lt <*> pure lk
renameType t@(HsSpliceTy _ _) = pure t
renameType (HsDocTy lt doc) = HsDocTy <$> renameLType lt <*> pure doc
renameType (HsBangTy bang lt) = HsBangTy bang <$> renameLType lt
renameType t@(HsRecTy _) = pure t
renameType t@(HsCoreTy _) = pure t
renameType (HsExplicitListTy ip ph ltys) =
    HsExplicitListTy ip ph <$> renameLTypes ltys
renameType (HsExplicitTupleTy phs ltys) =
    HsExplicitTupleTy phs <$> renameLTypes ltys
renameType t@(HsTyLit _) = pure t
renameType (HsWildCardTy wc) = pure (HsWildCardTy wc)
renameType (HsAppsTy _) = error "HsAppsTy: Only used before renaming"


renameLType :: (Eq (IdP name), SetName (IdP name))
            => LHsType name -> Rename (IdP name) (LHsType name)
renameLType = located renameType


renameLTypes :: (Eq (IdP name), SetName (IdP name))
             => [LHsType name] -> Rename (IdP name) [LHsType name]
renameLTypes = mapM renameLType


renameContext :: (Eq (IdP name), SetName (IdP name))
              => HsContext name -> Rename (IdP name) (HsContext name)
renameContext = renameLTypes

renameBinder :: (Eq (IdP name), SetName (IdP name))
             => HsTyVarBndr name -> Rename (IdP name) (HsTyVarBndr name)
renameBinder (UserTyVar lname) = UserTyVar <$> located renameName lname
renameBinder (KindedTyVar lname lkind) =
  KindedTyVar <$> located renameName lname <*> located renameType lkind


-- | Core renaming logic.
renameName :: (Eq name, SetName name) => name -> Rename name name
renameName name = do
    RenameEnv { .. } <- get
    case Map.lookup (getName name) rneCtx of
      Nothing
        | Just headTv <- Map.lookup (getNameRep name) rneHeadFVs
        , headTv /= getName name -> freshName name
      Just name' -> return name'
      _ -> return name


-- | Generate fresh occurrence name, put it into context and return.
freshName :: SetName name => name -> Rename name name
freshName name = do
    taken <- takenNames
    let name' = setInternalNameRep (findFreshName taken rep) name
    modify $ \rne -> rne
      { rneCtx = Map.insert (getName name) name' (rneCtx rne) }
    return name'
  where
    nname = getName name
    rep = getNameRep nname


takenNames :: NamedThing name => Rename name (Set NameRep)
takenNames = do
    RenameEnv { .. } <- get
    return $ Set.unions [headReps rneHeadFVs, rneSigFVs, ctxElems rneCtx]
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
    [ stringNameRep $ str ++ show i | i :: Int <- [0..] ]
  where
    str = nameRepString name


located :: Functor f => (a -> f b) -> Located a -> f (Located b)
located f (L loc e) = L loc <$> f e


tyVarName :: HsTyVarBndr name -> IdP name
tyVarName (UserTyVar name) = unLoc name
tyVarName (KindedTyVar (L _ name) _) = name

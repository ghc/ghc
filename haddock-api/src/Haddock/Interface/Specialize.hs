{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Haddock.Interface.Specialize
    ( specializeInstHead
    ) where


import Haddock.GhcUtils ( hsTyVarBndrName )
import Haddock.Syb
import Haddock.Types

import GHC
import GHC.Types.Basic ( PromotionFlag(..) )
import GHC.Types.Name
import GHC.Data.FastString
import GHC.Builtin.Types ( listTyConName, unrestrictedFunTyConName )

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
specialize :: Data a => [(Name, HsType GhcRn)] -> a -> a
specialize specs = go spec_map0
  where
    go :: forall x. Data x => Map Name (HsType GhcRn) -> x -> x
    go spec_map = everywhereButType @Name $ mkT $ sugar . strip_kind_sig . specialize_ty_var spec_map

    strip_kind_sig :: HsType GhcRn -> HsType GhcRn
    strip_kind_sig (HsKindSig _ (L _ t) _) = t
    strip_kind_sig typ = typ

    specialize_ty_var :: Map Name (HsType GhcRn) -> HsType GhcRn -> HsType GhcRn
    specialize_ty_var spec_map (HsTyVar _ _ (L _ name'))
      | Just t <- Map.lookup name' spec_map = t
    specialize_ty_var _ typ = typ

    -- This is a tricky recursive definition. By adding in the specializations
    -- one by one, we should avoid infinite loops.
    spec_map0 = foldr (\(n,t) acc -> Map.insert n (go acc t) acc) mempty specs

{-# SPECIALIZE specialize :: [(Name, HsType GhcRn)] -> HsType GhcRn -> HsType GhcRn #-}

-- | Instantiate given binders with corresponding types.
--
-- Again, it is just a convenience function around 'specialize'. Note that
-- length of type list should be the same as the number of binders.
specializeTyVarBndrs :: Data a => LHsQTyVars GhcRn -> [HsType GhcRn] -> a -> a
specializeTyVarBndrs bndrs typs = specialize $ zip bndrs' typs
  where
    bndrs' = map (hsTyVarBndrName . unLoc) . hsq_explicit $ bndrs



specializePseudoFamilyDecl :: LHsQTyVars GhcRn -> [HsType GhcRn]
                           -> PseudoFamilyDecl GhcRn
                           -> PseudoFamilyDecl GhcRn
specializePseudoFamilyDecl bndrs typs decl =
  decl {pfdTyVars = map (fmap (specializeTyVarBndrs bndrs typs)) (pfdTyVars decl)}

specializeSig :: LHsQTyVars GhcRn -> [HsType GhcRn]
              -> Sig GhcRn
              -> Sig GhcRn
specializeSig bndrs typs (TypeSig _ lnames typ) =
  TypeSig noAnn lnames (typ {hswc_body = noLocA typ'})
  where
    true_type :: HsSigType GhcRn
    true_type = unLoc (dropWildCards typ)
    typ' :: HsSigType GhcRn
    typ' = rename fv $ specializeTyVarBndrs bndrs typs true_type
    fv = foldr Set.union Set.empty . map freeVariablesType $ typs
specializeSig _ _ sig = sig


-- | Make all details of instance head (signatures, associated types)
-- specialized to that particular instance type.
specializeInstHead :: InstHead GhcRn -> InstHead GhcRn
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
sugar :: HsType GhcRn -> HsType GhcRn
sugar = sugarOperators . sugarTuples . sugarLists

sugarLists :: NamedThing (IdP (GhcPass p)) => HsType (GhcPass p) -> HsType (GhcPass p)
sugarLists (HsAppTy _ (L _ (HsTyVar _ _ (L _ name))) ltyp)
    | getName name == listTyConName = HsListTy noAnn ltyp
sugarLists typ = typ


sugarTuples :: NamedThing (IdP (GhcPass p)) => HsType (GhcPass p) -> HsType (GhcPass p)
sugarTuples typ =
    aux [] typ
  where
    aux apps (HsAppTy _ (L _ ftyp) atyp) = aux (atyp:apps) ftyp
    aux apps (HsParTy _ (L _ typ')) = aux apps typ'
    aux apps (HsTyVar _ _ (L _ name))
        | isBuiltInSyntax name' && suitable = HsTupleTy noAnn HsBoxedOrConstraintTuple apps
      where
        name' = getName name
        strName = getOccString name
        suitable = case parseTupleArity strName of
            Just arity -> arity == length apps
            Nothing -> False
    aux _ _ = typ


sugarOperators :: HsType GhcRn -> HsType GhcRn
sugarOperators (HsAppTy _ (L _ (HsAppTy _ (L _ (HsTyVar _ prom (L l name))) la)) lb)
    | isSymOcc $ getOccName name' = mkHsOpTy prom la (L l name) lb
    | unrestrictedFunTyConName == name' = HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) la lb
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


-- | Compute set of free variables of a given 'HsType'.
freeVariablesType :: HsType GhcRn -> Set Name
freeVariablesType =
    everythingWithState Set.empty Set.union
      (mkQ (\ctx -> (Set.empty, ctx)) queryType)

-- | Compute set of free variables of a given 'HsType'.
freeVariablesSigType :: HsSigType GhcRn -> Set Name
freeVariablesSigType =
    everythingWithState Set.empty Set.union
      (mkQ (\ctx -> (Set.empty, ctx)) queryType `extQ` querySigType)

queryType :: HsType GhcRn -> Set Name -> (Set Name, Set Name)
queryType term ctx = case term of
    HsForAllTy _ tele _ ->
        (Set.empty, Set.union ctx (teleNames tele))
    HsTyVar _ _ (L _ name)
        | getName name `Set.member` ctx -> (Set.empty, ctx)
        | otherwise -> (Set.singleton $ getName name, ctx)
    _ -> (Set.empty, ctx)
  where
    teleNames :: HsForAllTelescope GhcRn -> Set Name
    teleNames (HsForAllVis   _ bndrs) = bndrsNames bndrs
    teleNames (HsForAllInvis _ bndrs) = bndrsNames bndrs

querySigType :: HsSigType GhcRn -> Set Name -> (Set Name, Set Name)
querySigType (HsSig { sig_bndrs = outer_bndrs }) ctx =
  (Set.empty, Set.union ctx (bndrsNames (hsOuterExplicitBndrs outer_bndrs)))

bndrsNames :: [LHsTyVarBndr flag GhcRn] -> Set Name
bndrsNames = Set.fromList . map (getName . tyVarName . unLoc)


-- | Make given type visually unambiguous.
--
-- After applying 'specialize' method, some free type variables may become
-- visually ambiguous - for example, having @a -> b@ and specializing @a@ to
-- @(a -> b)@ we get @(a -> b) -> b@ where first occurrence of @b@ refers to
-- different type variable than latter one. Applying 'rename' function
-- will fix that type to be visually unambiguous again (making it something
-- like @(a -> b0) -> b@).
rename :: Set Name -> HsSigType GhcRn -> HsSigType GhcRn
rename fv typ = evalState (renameSigType typ) env
  where
    env = RenameEnv
      { rneHeadFVs = Map.fromList . map mkPair . Set.toList $ fv
      , rneSigFVs = Set.map getNameRep $ freeVariablesSigType typ
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


renameSigType :: HsSigType GhcRn -> Rename (IdP GhcRn) (HsSigType GhcRn)
renameSigType (HsSig x bndrs body) =
  HsSig x <$> renameOuterTyVarBndrs bndrs <*> renameLType body

renameOuterTyVarBndrs :: HsOuterTyVarBndrs flag GhcRn
                      -> Rename (IdP GhcRn) (HsOuterTyVarBndrs flag GhcRn)
renameOuterTyVarBndrs (HsOuterImplicit imp_tvs) =
  HsOuterImplicit <$> mapM renameName imp_tvs
renameOuterTyVarBndrs (HsOuterExplicit x exp_bndrs) =
  HsOuterExplicit x <$> mapM renameLBinder exp_bndrs

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
renameType (HsOpTy x prom la lop lb) =
    HsOpTy x prom <$> renameLType la <*> locatedN renameName lop <*> renameLType lb
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

renameHsArrow :: HsArrow GhcRn -> Rename (IdP GhcRn) (HsArrow GhcRn)
renameHsArrow (HsExplicitMult pct p arr) = (\p' -> HsExplicitMult pct p' arr) <$> renameLType p
renameHsArrow mult = pure mult


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

renameForAllTelescope :: HsForAllTelescope GhcRn
                      -> Rename (IdP GhcRn) (HsForAllTelescope GhcRn)
renameForAllTelescope (HsForAllVis x bndrs) =
  HsForAllVis x <$> mapM renameLBinder bndrs
renameForAllTelescope (HsForAllInvis x bndrs) =
  HsForAllInvis x <$> mapM renameLBinder bndrs

renameBinder :: HsTyVarBndr flag GhcRn -> Rename (IdP GhcRn) (HsTyVarBndr flag GhcRn)
renameBinder (UserTyVar x fl lname) = UserTyVar x fl <$> locatedN renameName lname
renameBinder (KindedTyVar x fl lname lkind) =
  KindedTyVar x fl <$> locatedN renameName lname <*> located renameType lkind

renameLBinder :: LHsTyVarBndr flag GhcRn -> Rename (IdP GhcRn) (LHsTyVarBndr flag GhcRn)
renameLBinder = located renameBinder

-- | Core renaming logic.
renameName :: SetName name => name -> Rename name name
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


located :: Functor f => (a -> f b) -> GenLocated l a -> f (GenLocated l b)
located f (L loc e) = L loc <$> f e

locatedN :: Functor f => (a -> f b) -> LocatedN a -> f (LocatedN b)
locatedN f (L loc e) = L loc <$> f e


tyVarName :: HsTyVarBndr flag GhcRn -> IdP GhcRn
tyVarName (UserTyVar _ _ name) = unLoc name
tyVarName (KindedTyVar _ _ (L _ name) _) = name

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module Haddock.Backends.Xhtml.Specialize
    ( specialize, specialize'
    , specializeTyVarBndrs
    , sugar, rename
    , freeVariables
    ) where


import Haddock.Syb
import Haddock.Types

import GHC
import Name
import FastString

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Data
import qualified Data.List as List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


-- | Instantiate all occurrences of given name with particular type.
specialize :: (Eq name, Typeable name)
           => Data a
           => name -> HsType name -> a -> a
specialize name details =
    everywhere $ mkT step
  where
    step (HsTyVar name') | name == name' = details
    step typ = typ


-- | Instantiate all occurrences of given names with corresponding types.
--
-- It is just a convenience function wrapping 'specialize' that supports more
-- that one specialization.
specialize' :: (Eq name, Typeable name)
            => Data a
            => [(name, HsType name)] -> a -> a
specialize' = flip $ foldr (uncurry specialize)


-- | Instantiate given binders with corresponding types.
--
-- Again, it is just a convenience function around 'specialize'. Note that
-- length of type list should be the same as the number of binders.
specializeTyVarBndrs :: (Eq name, Typeable name, DataId name)
                     => LHsTyVarBndrs name -> [HsType name]
                     -> HsType name -> HsType name
specializeTyVarBndrs bndrs typs =
    specialize' $ zip bndrs' typs
  where
    bndrs' = map (bname . unLoc) . hsq_tvs $ bndrs
    bname (UserTyVar name) = name
    bname (KindedTyVar (L _ name) _) = name


-- | Make given type use tuple and list literals where appropriate.
--
-- After applying 'specialize' function some terms may not use idiomatic list
-- and tuple literals resulting in types like @[] a@ or @(,,) a b c@. This
-- can be fixed using 'sugar' function, that will turn such types into @[a]@
-- and @(a, b, c)@.
sugar :: forall name. (NamedThing name, DataId name)
      => HsType name -> HsType name
sugar =
    everywhere $ mkT step
  where
    step :: HsType name -> HsType name
    step = sugarTuples . sugarLists


sugarLists :: NamedThing name => HsType name -> HsType name
sugarLists (HsAppTy (L _ (HsTyVar name)) ltyp)
    | isBuiltInSyntax name' && strName == "[]" = HsListTy ltyp
  where
    name' = getName name
    strName = occNameString . nameOccName $ name'
sugarLists typ = typ


sugarTuples :: NamedThing name => HsType name -> HsType name
sugarTuples typ =
    aux [] typ
  where
    aux apps (HsAppTy (L _ ftyp) atyp) = aux (atyp:apps) ftyp
    aux apps (HsParTy (L _ typ')) = aux apps typ'
    aux apps (HsTyVar name)
        | isBuiltInSyntax name' && suitable = HsTupleTy HsBoxedTuple apps
      where
        name' = getName name
        strName = occNameString . nameOccName $ name'
        suitable = case parseTupleArity strName of
            Just arity -> arity == length apps
            Nothing -> False
    aux _ _ = typ


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
freeVariables :: forall name. (NamedThing name, DataId name)
              => HsType name -> Set NameRep
freeVariables =
    everythingWithState Set.empty Set.union query
  where
    query term ctx = case cast term :: Maybe (HsType name) of
        Just (HsForAllTy _ _ bndrs _ _) ->
            (Set.empty, Set.union ctx (bndrsNames bndrs))
        Just (HsTyVar name)
            | getName name `Set.member` ctx -> (Set.empty, ctx)
            | otherwise -> (Set.singleton $ getNameRep name, ctx)
        _ -> (Set.empty, ctx)
    bndrsNames = Set.fromList . map (getName . tyVarName . unLoc) . hsq_tvs


-- | Make given type visually unambiguous.
--
-- After applying 'specialize' method, some free type variables may become
-- visually ambiguous - for example, having @a -> b@ and specializing @a@ to
-- @(a -> b)@ we get @(a -> b) -> b@ where first occurrence of @b@ refers to
-- different type variable than latter one. Applying 'rename' function
-- will fix that type to be visually unambiguous again (making it something
-- like @(a -> c) -> b@).
rename :: SetName name => Set NameRep -> HsType name -> HsType name
rename fv typ = runReader (renameType typ) $ RenameEnv
    { rneFV = fv
    , rneCtx = Map.empty
    }


-- | Renaming monad.
type Rename name = Reader (RenameEnv name)

-- | Binding generation monad.
type Rebind name = State (RenameEnv name)

data RenameEnv name = RenameEnv
    { rneFV :: Set NameRep
    , rneCtx :: Map Name name
    }


renameType :: SetName name => HsType name -> Rename name (HsType name)
renameType (HsForAllTy ex mspan lbndrs lctx lt) = rebind lbndrs $ \lbndrs' ->
    HsForAllTy
        <$> pure ex
        <*> pure mspan
        <*> pure lbndrs'
        <*> located renameContext lctx
        <*> renameLType lt
renameType (HsTyVar name) = HsTyVar <$> renameName name
renameType (HsAppTy lf la) = HsAppTy <$> renameLType lf <*> renameLType la
renameType (HsFunTy la lr) = HsFunTy <$> renameLType la <*> renameLType lr
renameType (HsListTy lt) = HsListTy <$> renameLType lt
renameType (HsPArrTy lt) = HsPArrTy <$> renameLType lt
renameType (HsTupleTy srt lt) = HsTupleTy srt <$> mapM renameLType lt
renameType (HsOpTy la lop lb) =
    HsOpTy <$> renameLType la <*> renameLTyOp lop <*> renameLType lb
renameType (HsParTy lt) = HsParTy <$> renameLType lt
renameType (HsIParamTy ip lt) = HsIParamTy ip <$> renameLType lt
renameType (HsEqTy la lb) = HsEqTy <$> renameLType la <*> renameLType lb
renameType (HsKindSig lt lk) = HsKindSig <$> renameLType lt <*> pure lk
renameType t@(HsQuasiQuoteTy _) = pure t
renameType t@(HsSpliceTy _ _) = pure t
renameType (HsDocTy lt doc) = HsDocTy <$> renameLType lt <*> pure doc
renameType (HsBangTy bang lt) = HsBangTy bang <$> renameLType lt
renameType t@(HsRecTy _) = pure t
renameType t@(HsCoreTy _) = pure t
renameType (HsExplicitListTy ph ltys) =
    HsExplicitListTy ph <$> renameLTypes ltys
renameType (HsExplicitTupleTy phs ltys) =
    HsExplicitTupleTy phs <$> renameLTypes ltys
renameType t@(HsTyLit _) = pure t
renameType (HsWrapTy wrap t) = HsWrapTy wrap <$> renameType t
renameType HsWildcardTy = pure HsWildcardTy
renameType (HsNamedWildcardTy name) = HsNamedWildcardTy <$> renameName name


renameLType :: SetName name => LHsType name -> Rename name (LHsType name)
renameLType = located renameType


renameLTypes :: SetName name => [LHsType name] -> Rename name [LHsType name]
renameLTypes = mapM renameLType


renameContext :: SetName name => HsContext name -> Rename name (HsContext name)
renameContext = renameLTypes


renameLTyOp :: SetName name => LHsTyOp name -> Rename name (LHsTyOp name)
renameLTyOp (wrap, lname) = (,) wrap <$> located renameName lname


renameName :: SetName name => name -> Rename name name
renameName name = do
    RenameEnv { rneCtx = ctx } <- ask
    pure $ case Map.lookup (getName name) ctx of
        Just name' -> name'
        Nothing -> name


rebind :: SetName name
       => LHsTyVarBndrs name -> (LHsTyVarBndrs name -> Rename name a)
       -> Rename name a
rebind lbndrs action = do
    (lbndrs', env') <- runState (rebindLTyVarBndrs lbndrs) <$> ask
    local (const env') (action lbndrs')


rebindLTyVarBndrs :: SetName name
                  => LHsTyVarBndrs name -> Rebind name (LHsTyVarBndrs name)
rebindLTyVarBndrs lbndrs = do
    tys' <- mapM (located rebindTyVarBndr) $ hsq_tvs lbndrs
    pure $ lbndrs { hsq_tvs = tys' }


rebindTyVarBndr :: SetName name
                => HsTyVarBndr name -> Rebind name (HsTyVarBndr name)
rebindTyVarBndr (UserTyVar name) =
    UserTyVar <$> rebindName name
rebindTyVarBndr (KindedTyVar name kinds) =
    KindedTyVar <$> located rebindName name <*> pure kinds


rebindName :: SetName name => name -> Rebind name name
rebindName name = do
    RenameEnv { .. } <- get
    taken <- takenNames
    case Map.lookup (getName name) rneCtx of
        Just name' -> pure name'
        Nothing | getNameRep name `Set.member` taken -> freshName name
        Nothing -> reuseName name


-- | Generate fresh occurrence name, put it into context and return.
freshName :: SetName name => name -> Rebind name name
freshName name = do
    env@RenameEnv { .. } <- get
    taken <- takenNames
    let name' = setInternalNameRep (findFreshName taken rep) name
    put $ env { rneCtx = Map.insert nname name' rneCtx }
    return name'
  where
    nname = getName name
    rep = getNameRep nname


reuseName :: SetName name => name -> Rebind name name
reuseName name = do
    env@RenameEnv { .. } <- get
    put $ env { rneCtx = Map.insert (getName name) name rneCtx }
    return name


takenNames :: NamedThing name => Rebind name (Set NameRep)
takenNames = do
    RenameEnv { .. } <- get
    return $ Set.union rneFV (ctxElems rneCtx)
  where
    ctxElems = Set.fromList . map getNameRep . Map.elems


findFreshName :: Set NameRep -> NameRep -> NameRep
findFreshName taken =
    fromJust . List.find isFresh . alternativeNames
  where
    isFresh = not . flip Set.member taken


alternativeNames :: NameRep -> [NameRep]
alternativeNames name
    | [_] <- nameRepString name = letterNames ++ alternativeNames' name
  where
    letterNames = map (stringNameRep . pure) ['a'..'z']
alternativeNames name = alternativeNames' name


alternativeNames' :: NameRep -> [NameRep]
alternativeNames' name =
    [ stringNameRep $ str ++ show i | i :: Int <- [0..] ]
  where
    str = nameRepString name


located :: Functor f => (a -> f b) -> Located a -> f (Located b)
located f (L loc e) = L loc <$> f e


tyVarName :: HsTyVarBndr name -> name
tyVarName (UserTyVar name) = name
tyVarName (KindedTyVar (L _ name) _) = name

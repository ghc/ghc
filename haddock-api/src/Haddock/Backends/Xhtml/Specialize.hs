{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Haddock.Backends.Xhtml.Specialize
    ( specialize, specialize'
    , specializeTyVarBndrs
    , sugar, rename
    ) where


import Haddock.Syb

import GHC
import Name

import Control.Monad
import Control.Monad.Trans.RWS

import Data.Data
import qualified Data.List as List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


specialize :: (Eq name, Typeable name)
           => Data a
           => name -> HsType name -> a -> a
specialize name details = everywhere (mkT $ specializeStep name details)


specialize' :: (Eq name, Typeable name)
            => Data a
            => [(name, HsType name)] -> a -> a
specialize' = flip $ foldr (uncurry specialize)


specializeStep :: Eq name => name -> HsType name -> HsType name -> HsType name
specializeStep name details (HsTyVar name') | name == name' = details
specializeStep _ _ typ = typ


specializeTyVarBndrs :: (Eq name, Typeable name, DataId name)
                     => LHsTyVarBndrs name -> [HsType name]
                     -> HsType name -> HsType name
specializeTyVarBndrs bndrs typs =
    specialize' $ zip bndrs' typs
  where
    bndrs' = map (bname . unLoc) . hsq_tvs $ bndrs
    bname (UserTyVar name) = name
    bname (KindedTyVar (L _ name) _) = name


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


class NamedThing name => SetName name where

    setName :: Name -> name -> name


setInternalOccName :: SetName name => OccName -> name -> name
setInternalOccName occ name =
    setName nname' name
  where
    nname = getName name
    nname' = mkInternalName (nameUnique nname) occ (nameSrcSpan nname)


rename :: SetName name => HsType name -> HsType name
rename = fst . evalRWS undefined Map.empty . renameType -- TODO.


type Rename name a = RWS (Set OccName) () (Map Name name) a


renameType :: SetName name => HsType name -> Rename name (HsType name)
renameType (HsForAllTy ex mspan lbndrs lctx lt) = do
    lbndrs' <- renameLTyVarBndrs lbndrs
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


renameContext :: SetName name => HsContext name
              -> Rename name (HsContext name)
renameContext = renameLTypes


renameLTyVarBndrs :: SetName name => LHsTyVarBndrs name -> Rename name (LHsTyVarBndrs name)
renameLTyVarBndrs lbndrs = do
    tys' <- mapM (located renameTyVarBndr) $ hsq_tvs lbndrs
    pure $ lbndrs { hsq_tvs = tys' }


renameTyVarBndr :: SetName name => HsTyVarBndr name
                -> Rename name (HsTyVarBndr name)
renameTyVarBndr (UserTyVar name) =
    UserTyVar <$> renameNameBndr name
renameTyVarBndr (KindedTyVar name kinds) =
    KindedTyVar <$> located renameNameBndr name <*> pure kinds


renameLTyOp :: SetName name => LHsTyOp name -> Rename name (LHsTyOp name)
renameLTyOp (wrap, lname) = (,) wrap <$> located renameName lname


renameNameBndr :: SetName name => name -> Rename name name
renameNameBndr name = do
    fv <- ask
    env <- get
    case Map.lookup (getName name) env of
        Just name' -> pure name'
        Nothing | getOccName name `Set.member` fv -> freshName name
        Nothing -> pure name


renameName :: SetName name => name -> Rename name name
renameName name = do
    env <- get
    pure $ case Map.lookup (getName name) env of
        Just name' -> name'
        Nothing -> name


freshName :: SetName name => name -> Rename name name
freshName name = do
    fv <- ask
    env <- get
    let taken = Set.union fv (Set.fromList . map getOccName . Map.keys $ env)
    let name' = setInternalOccName (findFreshName taken occ) name
    put $ Map.insert nname name' env
    return name'
  where
    nname = getName name
    occ = nameOccName nname


findFreshName :: Set OccName -> OccName -> OccName
findFreshName taken =
    fromJust . List.find isFresh . alternativeNames
  where
    isFresh = not . flip Set.member taken


alternativeNames :: OccName -> [OccName]
alternativeNames name =
    [ mkVarOcc $ str ++ show i | i :: Int <- [0..] ]
  where
    str = occNameString name


located :: Functor f => (a -> f b) -> Located a -> f (Located b)
located f (L loc e) = L loc <$> f e

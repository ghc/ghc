{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Haddock.Backends.Xhtml.Specialize
    ( specialize, specialize'
    , specializeTyVarBndrs
    , sugar
    ) where


import Haddock.Syb

import GHC
import Name

import Control.Monad
import Data.Data


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


sugar :: (NamedThing name, DataId name) => HsType name -> HsType name
sugar = sugarTuples . sugarLists


sugarLists :: forall name. (NamedThing name, DataId name)
           => HsType name -> HsType name
sugarLists = everywhere $ mkT (sugarListsStep :: HsType name -> HsType name)


sugarListsStep :: NamedThing name => HsType name -> HsType name
sugarListsStep (HsAppTy (L _ (HsTyVar name)) ltyp)
    | isBuiltInSyntax name' && strName == "[]" = HsListTy ltyp
  where
    name' = getName name
    strName = occNameString . nameOccName $ name'
sugarListsStep typ = typ


sugarTuples :: forall name. (NamedThing name, DataId name)
            => HsType name -> HsType name
sugarTuples = everywhere $
    mkT (sugarTuplesStep :: HsType name -> HsType name)


sugarTuplesStep :: NamedThing name => HsType name -> HsType name
sugarTuplesStep typ =
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

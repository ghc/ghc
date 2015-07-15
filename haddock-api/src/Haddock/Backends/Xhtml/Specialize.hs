{-# LANGUAGE FlexibleContexts #-}


module Haddock.Backends.Xhtml.Specialize
    ( specialize, specialize'
    , specializeTyVarBndrs
    ) where


import Haddock.Syb

import GHC

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

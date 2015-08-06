{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc, OutputableBndr, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import PlaceHolder ( DataId )
import Data.Data hiding ( Fixity )

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal representational
type role GRHSs nominal representational
type role HsSplice nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)

instance Typeable HsSplice
instance Typeable HsExpr
instance Typeable MatchGroup
instance Typeable GRHSs

instance (DataId id) => Data (HsSplice id)
instance (DataId id) => Data (HsExpr id)
instance (DataId id) => Data (HsCmd id)
instance (Data body,DataId id) => Data (MatchGroup id body)
instance (Data body,DataId id) => Data (GRHSs id body)

instance OutputableBndr id => Outputable (HsExpr id)
instance OutputableBndr id => Outputable (HsCmd id)

type LHsExpr a = Located (HsExpr a)
type SyntaxExpr a = HsExpr a

pprLExpr :: (OutputableBndr i) =>
        LHsExpr i -> SDoc

pprExpr :: (OutputableBndr i) =>
        HsExpr i -> SDoc

pprSplice :: (OutputableBndr i) => HsSplice i -> SDoc

pprPatBind :: (OutputableBndr bndr, OutputableBndr id, Outputable body)
           => LPat bndr -> GRHSs id body -> SDoc

pprFunBind :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
           => idL -> Bool -> MatchGroup idR body -> SDoc

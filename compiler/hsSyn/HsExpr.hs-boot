{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import HsExtension ( OutputableBndrId, DataIdLR, GhcPass )
import Data.Data hiding ( Fixity )

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal representational
type role GRHSs nominal representational
type role HsSplice nominal
type role SyntaxExpr nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)
data SyntaxExpr (i :: *)

instance (DataIdLR p p) => Data (HsSplice p)
instance (DataIdLR p p) => Data (HsExpr p)
instance (DataIdLR p p) => Data (HsCmd p)
instance (Data body,DataIdLR p p) => Data (MatchGroup p body)
instance (Data body,DataIdLR p p) => Data (GRHSs p body)
instance (DataIdLR p p) => Data (SyntaxExpr p)

instance (OutputableBndrId (GhcPass p)) => Outputable (HsExpr (GhcPass p))
instance (OutputableBndrId (GhcPass p)) => Outputable (HsCmd (GhcPass p))

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndrId (GhcPass p)) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (OutputableBndrId (GhcPass p)) => HsExpr (GhcPass p) -> SDoc

pprSplice :: (OutputableBndrId (GhcPass p)) => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (OutputableBndrId (GhcPass p))
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p body. (OutputableBndrId (GhcPass bndr),
                                   OutputableBndrId (GhcPass p),
                                   Outputable body)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc

pprFunBind :: (OutputableBndrId (GhcPass idR), Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc

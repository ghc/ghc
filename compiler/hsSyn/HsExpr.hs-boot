{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import HsExtension ( OutputableBndrId, DataId, SourceTextX )
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

instance (DataId p) => Data (HsSplice p)
instance (DataId p) => Data (HsExpr p)
instance (DataId p) => Data (HsCmd p)
instance (Data body,DataId p) => Data (MatchGroup p body)
instance (Data body,DataId p) => Data (GRHSs p body)
instance (DataId p) => Data (SyntaxExpr p)

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsExpr p)
instance (SourceTextX p, OutputableBndrId p) => Outputable (HsCmd p)

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (SourceTextX p, OutputableBndrId p) => LHsExpr p -> SDoc

pprExpr :: (SourceTextX p, OutputableBndrId p) => HsExpr p -> SDoc

pprSplice :: (SourceTextX p, OutputableBndrId p) => HsSplice p -> SDoc

pprSpliceDecl ::  (SourceTextX p, OutputableBndrId p)
          => HsSplice p -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p body. (SourceTextX p, SourceTextX bndr,
                                   OutputableBndrId bndr,
                                   OutputableBndrId p,
                                   Outputable body)
           => LPat bndr -> GRHSs p body -> SDoc

pprFunBind :: (SourceTextX idR, OutputableBndrId idR, Outputable body)
           => MatchGroup idR body -> SDoc

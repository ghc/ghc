{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module GHC.Hs.PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Expr where

import Data.Foldable (Foldable)
import Data.List.NonEmpty (NonEmpty)

import SrcLoc     ( Located )
import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} GHC.Hs.Pat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import GHC.Hs.Extension ( OutputableBndrId, GhcPass )

type role HsExpr nominal
type role HsCmd nominal
-- TODO make first param (a functor) representational
type role MatchGroup' nominal nominal nominal
type role GRHSs nominal nominal
type role HsSplice nominal
type role SyntaxExpr nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup' (f :: * -> *) (a :: *) (body :: *)
type MatchGroup = MatchGroup' NonEmpty
type MatchGroup0 = MatchGroup' []
data GRHSs (a :: *) (body :: *)
data SyntaxExpr (i :: *)

instance OutputableBndrId p => Outputable (HsExpr (GhcPass p))
instance OutputableBndrId p => Outputable (HsCmd (GhcPass p))

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc

pprSplice :: (OutputableBndrId p) => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (OutputableBndrId p)
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p body. (OutputableBndrId bndr,
                                   OutputableBndrId p,
                                   Outputable body)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc

pprFunBind :: (Foldable f, OutputableBndrId idR, Outputable body)
           => MatchGroup' f (GhcPass idR) body -> SDoc

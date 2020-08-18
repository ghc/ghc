{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Expr where

import GHC.Utils.Outputable ( SDoc, Outputable )
import {-# SOURCE #-} GHC.Hs.Pat  ( LPat )
import GHC.Types.Basic  ( SpliceExplicitFlag(..))
import GHC.Hs.Extension (OutputableBndrId, GhcPass, XRec )
import Data.Kind  ( Type )

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal nominal
type role GRHSs nominal nominal
type role HsSplice nominal
data HsExpr (i :: Type)
data HsCmd  (i :: Type)
data HsSplice (i :: Type)
data MatchGroup (a :: Type) (body :: Type)
data GRHSs (a :: Type) (body :: Type)
type family SyntaxExpr (i :: Type)

instance (OutputableBndrId p) => Outputable (HsExpr (GhcPass p))
instance (OutputableBndrId p) => Outputable (HsCmd (GhcPass p))

type LHsExpr a = XRec a (HsExpr a)
-- type LHsExpr a = LocatedA (HsExpr a)
                       -- AZ: old one

pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc

pprSplice :: (OutputableBndrId p) => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (OutputableBndrId p)
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p . (OutputableBndrId bndr,
                               OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc

pprFunBind :: (OutputableBndrId idR)
           => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc

{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc', Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import HsExtension ( OutputableBndrId, GhcPass )

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal nominal
type role GRHSs nominal nominal
type role HsSplice nominal
type role SyntaxExpr nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)
data SyntaxExpr (i :: *)

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsExpr p)
instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsCmd p)

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndrId (GhcPass p)) => LHsExpr (GhcPass p) -> SDoc' r

pprExpr :: (OutputableBndrId (GhcPass p)) => HsExpr (GhcPass p) -> SDoc' r

pprSplice :: (OutputableBndrId (GhcPass p)) => HsSplice (GhcPass p) -> SDoc' r

pprSpliceDecl ::  (OutputableBndrId (GhcPass p))
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc' r

pprPatBind
  :: forall bndr p body r
  .  ( OutputableBndrId (GhcPass bndr)
     , OutputableBndrId (GhcPass p)
     , Outputable body
     )
  => LPat (GhcPass bndr)
  -> GRHSs (GhcPass p) body
  -> SDoc' r

pprFunBind
  :: ( OutputableBndrId (GhcPass idR)
     , Outputable body
     )
  => MatchGroup (GhcPass idR) body
  -> SDoc' r

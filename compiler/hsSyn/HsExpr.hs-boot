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
import Outputable ( Outputable, OutputableNeedsOfConfig, SDoc' )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import HsExtension
  ( OutputableBndrId, OutputableBndrIdNeedsOfConfig
  , GhcPass
  )

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

pprLExpr
  :: ( OutputableBndrId (GhcPass p)
     , OutputableBndrIdNeedsOfConfig (GhcPass p) r
     )
  => LHsExpr (GhcPass p) -> SDoc' r

pprExpr
  :: ( OutputableBndrId (GhcPass p)
     , OutputableBndrIdNeedsOfConfig (GhcPass p) r
     )
  => HsExpr (GhcPass p) -> SDoc' r

pprSplice
  :: ( OutputableBndrId (GhcPass p)
     , OutputableBndrIdNeedsOfConfig (GhcPass p) r
     )
  => HsSplice (GhcPass p) -> SDoc' r

pprSpliceDecl
  :: ( OutputableBndrId (GhcPass p)
     , OutputableBndrIdNeedsOfConfig (GhcPass p) r
     )
  => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc' r

pprPatBind
  :: forall bndr p body r
  .  ( OutputableBndrId (GhcPass bndr)
     , OutputableBndrId (GhcPass p)
     , Outputable body
     , OutputableBndrIdNeedsOfConfig (GhcPass bndr) r
     , OutputableBndrIdNeedsOfConfig (GhcPass p) r
     , OutputableNeedsOfConfig body r
     )
  => LPat (GhcPass bndr)
  -> GRHSs (GhcPass p) body
  -> SDoc' r

pprFunBind
  :: ( OutputableBndrId (GhcPass idR)
     , Outputable body
     , OutputableBndrIdNeedsOfConfig (GhcPass idR) r
     , OutputableNeedsOfConfig body r
     )
  => MatchGroup (GhcPass idR) body
  -> SDoc' r

-- Work around missing `OutputableNeedsOfConfig (LHsExpr (GhcPass idR))` by
-- specializating `body`.
pprFunBind'
  :: ( OutputableBndrId (GhcPass idR)
     , OutputableBndrIdNeedsOfConfig (GhcPass idR) r
     )
  => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR))
  -> SDoc' r

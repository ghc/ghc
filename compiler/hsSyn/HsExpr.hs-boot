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
import HsExtension ( OutputableBndrId, DataId, DataIdLR, SourceTextX, GhcPass )
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

instance (DataIdLR id id, DataIF id) => Data (HsSplice id)
instance (DataIdLR p p, DataIF p) => Data (HsExpr p)
instance (DataIdLR id id, DataIF id) => Data (HsCmd id)
instance (Data body,DataIF p) => Data (MatchGroup p body)
instance (Data body,DataIdLR p p, DataIF p) => Data (GRHSs p body)
instance (DataIdLR p p, DataIF p) => Data (SyntaxExpr p)

instance (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
       => Outputable (HsExpr (GhcPass p))
instance (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
       => Outputable (HsCmd (GhcPass p))

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
         => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
        => HsExpr (GhcPass p) -> SDoc

pprSplice :: (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
          => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p body. (SourceTextX (GhcPass p),
                                   SourceTextX (GhcPass bndr),
                                   OutputableBndrId (GhcPass bndr),
                                   OutputableBndrId (GhcPass p),
                                   Outputable body)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc

pprFunBind :: (SourceTextX (GhcPass idR), OutputableBndrId (GhcPass idR),
               Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc

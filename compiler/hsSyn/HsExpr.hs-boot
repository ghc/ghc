{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import PlaceHolder ( DataId, OutputableBndrId, HasOccNameId )
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

instance (DataId id) => Data (HsSplice id)
instance (DataId id) => Data (HsExpr id)
instance (DataId id) => Data (HsCmd id)
instance (Data body,DataId id) => Data (MatchGroup id body)
instance (Data body,DataId id) => Data (GRHSs id body)
instance (DataId id) => Data (SyntaxExpr id)

instance (OutputableBndrId id, HasOccNameId id) => Outputable (HsExpr id)
instance (OutputableBndrId id, HasOccNameId id) => Outputable (HsCmd id)

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndrId id, HasOccNameId id) => LHsExpr id -> SDoc

pprExpr :: (OutputableBndrId id,HasOccNameId id) => HsExpr id -> SDoc

pprSplice :: (OutputableBndrId id, HasOccNameId id)
          => HsSplice id -> SDoc

pprSpliceDecl ::  (OutputableBndrId id, HasOccNameId id)
          => HsSplice id -> SpliceExplicitFlag -> SDoc

pprPatBind :: (OutputableBndrId bndr,
               OutputableBndrId id,
               HasOccNameId id,
               HasOccNameId bndr,
               Outputable body)
           => LPat bndr -> GRHSs id body -> SDoc

pprFunBind :: (OutputableBndrId idR, HasOccNameId idR, Outputable body)
           => MatchGroup idR body -> SDoc

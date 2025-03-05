{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Expr where

import GHC.Utils.Outputable ( SDoc, Outputable )
import Language.Haskell.Syntax.Pat ( LPat )
import {-# SOURCE #-} GHC.Hs.Pat () -- for Outputable
import Language.Haskell.Syntax.Expr
  ( HsExpr, LHsExpr
  , HsCmd
  , LMatchGroup
  , GRHSs
  , HsUntypedSplice
  )
import GHC.Hs.Extension ( OutputableBndrId, GhcPass )
import GHC.Types.Name   ( Name )
import Data.Bool  ( Bool )
import Data.Maybe ( Maybe )

type SplicePointName = Name

instance (OutputableBndrId p) => Outputable (HsExpr (GhcPass p))
instance (OutputableBndrId p) => Outputable (HsCmd (GhcPass p))

pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc

pprTypedSplice   :: (OutputableBndrId p) => Maybe SplicePointName -> LHsExpr (GhcPass p) -> SDoc
pprUntypedSplice :: (OutputableBndrId p) => Bool -> Maybe SplicePointName -> HsUntypedSplice (GhcPass p) -> SDoc

pprPatBind :: forall bndr p . (OutputableBndrId bndr,
                               OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc

pprFunBind :: (OutputableBndrId idR)
           => LMatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc

data ThModFinalizers
type role HsUntypedSpliceResult representational
data HsUntypedSpliceResult thing
  = HsUntypedSpliceTop
      { utsplice_result_finalizers :: ThModFinalizers
      , utsplice_result            :: thing
      }
  | HsUntypedSpliceNested SplicePointName

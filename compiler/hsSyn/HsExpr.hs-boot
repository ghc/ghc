{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HsExpr where

import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import HsExtension ( OutputableBndrId, DataId, SourceTextX, GHC )
import Data.Data hiding ( Fixity )
import qualified AST


type HsExpr     pass      = AST.Expr       (GHC pass)
type HsCmd      pass      = AST.Cmd        (GHC pass)
type HsSplice   pass      = AST.Splice     (GHC pass)
type MatchGroup pass body = AST.MatchGroup (GHC pass) body
type GRHSs      pass body = AST.GRHSs      (GHC pass) body
type LHsExpr    pass      = AST.LExpr    (GHC pass)

type role SyntaxExpr nominal
data SyntaxExpr (i :: *)

instance (DataId p) => Data (HsSplice p)
instance (DataId p) => Data (HsExpr p)
instance (DataId p) => Data (HsCmd p)
instance (Data body,DataId p) => Data (MatchGroup p body)
instance (Data body,DataId p) => Data (GRHSs p body)
instance (DataId p) => Data (SyntaxExpr p)

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsExpr p)
instance (SourceTextX p, OutputableBndrId p) => Outputable (HsCmd p)


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

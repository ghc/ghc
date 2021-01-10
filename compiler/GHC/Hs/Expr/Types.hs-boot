{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Expr.Types where

import GHC.Hs.Extension ( XRec )
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

type LHsExpr a = XRec a (HsExpr a)

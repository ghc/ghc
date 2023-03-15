{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.Utils
  ( assignToTypedExprs
  , assignCoerce1
  , assignToExprCtx
  )
where

import GHC.Prelude

import GHC.StgToJS.Types
import GHC.StgToJS.ExprCtx

import GHC.JS.Unsat.Syntax
import GHC.JS.Make

import GHC.Core.TyCon

import GHC.Utils.Panic
import GHC.Utils.Outputable

assignToTypedExprs :: [TypedExpr] -> [JExpr] -> JStat
assignToTypedExprs tes es =
  assignAllEqual (concatMap typex_expr tes) es

assignTypedExprs :: [TypedExpr] -> [TypedExpr] -> JStat
assignTypedExprs tes es =
  -- TODO: check primRep (typex_typ) here?
  assignToTypedExprs tes (concatMap typex_expr es)

assignToExprCtx :: ExprCtx -> [JExpr] -> JStat
assignToExprCtx ctx es = assignToTypedExprs (ctxTarget ctx) es

-- | Assign first expr only (if it exists), performing coercions between some
-- PrimReps (e.g. StablePtr# and Addr#).
assignCoerce1 :: [TypedExpr] -> [TypedExpr] -> JStat
assignCoerce1 [x] [y] = assignCoerce x y
assignCoerce1 []  []  = mempty
assignCoerce1 _x _y   = pprPanic "assignCoerce1"
                          (vcat [ text "lengths do not match"
                                -- FIXME: Outputable instance removed until JStg replaces JStat
                                -- , ppr x
                                -- , ppr y
                                ])

-- | Assign p2 to p1 with optional coercion
assignCoerce :: TypedExpr -> TypedExpr -> JStat
-- Coercion between StablePtr# and Addr#
assignCoerce (TypedExpr AddrRep [a_val, a_off]) (TypedExpr UnliftedRep [sptr]) = mconcat
    [ a_val |= var "h$stablePtrBuf"
    , a_off |= sptr
    ]
assignCoerce (TypedExpr UnliftedRep [sptr]) (TypedExpr AddrRep [_a_val, a_off]) =
  sptr |= a_off
assignCoerce p1 p2 = assignTypedExprs [p1] [p2]


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.DataCon
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--  Code generation of data constructors
-----------------------------------------------------------------------------

module GHC.StgToJS.DataCon
  ( genCon
  , allocCon
  , allocUnboxedCon
  , allocDynamicE
  , allocDynamic
  )
where

import GHC.Prelude

import GHC.JS.Unsat.Syntax
import GHC.JS.Transform
import GHC.JS.Make

import GHC.StgToJS.Closure
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Types
import GHC.StgToJS.Monad
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Profiling
import GHC.StgToJS.Utils
import GHC.StgToJS.Ids

import GHC.Core.DataCon

import GHC.Types.CostCentre

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Maybe

-- | Generate a data constructor. Special handling for unboxed tuples
genCon :: ExprCtx -> DataCon -> [JExpr] -> G JStat
genCon ctx con args
  | isUnboxedTupleDataCon con
  = return $ assignToExprCtx ctx args

  | [ValExpr (JVar ctxi)] <- concatMap typex_expr (ctxTarget ctx)
  = allocCon ctxi con currentCCS args

  | xs <- concatMap typex_expr (ctxTarget ctx)
  = pprPanic "genCon: unhandled DataCon" (ppr (con
                                              , fmap satJExpr args
                                              , fmap satJExpr xs
                                              ))

-- | Allocate a data constructor. Allocate in this context means bind the data
-- constructor to 'to'
allocCon :: Ident -> DataCon -> CostCentreStack -> [JExpr] -> G JStat
allocCon to con cc xs
  | isBoolDataCon con || isUnboxableCon con =
      return (toJExpr to |= allocUnboxedCon con xs)
{-  | null xs = do
      i <- varForId (dataConWorkId con)
      return (assignj to i) -}
  | otherwise = do
      e <- varForDataConWorker con
      cs <- getSettings
      prof <- profiling
      ccsJ <- if prof then ccsVarJ cc else return Nothing
      return $ allocDynamic cs False to e xs ccsJ

-- | Allocate an unboxed data constructor. If we have a bool we calculate the
-- right value. If not then we expect a singleton list and unbox by converting
-- ''C x' to 'x'. NB. This function may panic.
allocUnboxedCon :: DataCon -> [JExpr] -> JExpr
allocUnboxedCon con = \case
  []
    | isBoolDataCon con && dataConTag con == 1 -> false_
    | isBoolDataCon con && dataConTag con == 2 -> true_
  [x]
    | isUnboxableCon con -> x
  xs -> pprPanic "allocUnboxedCon: not an unboxed constructor" (ppr (con, fmap satJExpr xs))

-- | Allocate an entry function. See 'GHC.StgToJS.hs' for the object layout.
allocDynamicE :: Bool          -- ^ csInlineAlloc from StgToJSConfig
              -> JExpr
              -> [JExpr]
              -> Maybe JExpr
              -> JExpr
allocDynamicE  inline_alloc entry free cc
  | inline_alloc || length free > jsClosureCount
    = newClosure $ mkClosure entry free (ValExpr (JInt 0)) cc
  | otherwise = ApplExpr allocFun (toJExpr entry : free ++ maybeToList cc)
  where
    allocFun = allocClsA (length free)

-- | Allocate a dynamic object
allocDynamic :: StgToJSConfig -> Bool -> Ident -> JExpr -> [JExpr] -> Maybe JExpr -> JStat
allocDynamic s need_decl to entry free cc
  | need_decl = DeclStat to (Just value)
  | otherwise = toJExpr to |= value
    where
      value = allocDynamicE (csInlineAlloc s) entry free cc

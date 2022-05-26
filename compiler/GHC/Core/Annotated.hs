{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Annotated Core
module GHC.Core.Annotated (
        -- * Annotated expression data types
        AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt(..),

        -- ** Operations on annotated expressions
        collectAnnArgs, collectAnnArgsTicks,

        -- ** Operations on annotations
        deAnnotate, deAnnotate', deAnnAlt, deAnnBind,
        collectAnnBndrs, collectNAnnBndrs,
    ) where

import GHC.Prelude

import GHC.Types.Var
import GHC.Core
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Types.Literal
import GHC.Types.Tickish

import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Annotated core: allows annotation at every node in the tree
type AnnExpr bndr annot = (annot, AnnExpr' bndr annot)

-- | A clone of the 'Expr' type but allowing annotation at every tree node
data AnnExpr' bndr annot
  = AnnVar      Id
  | AnnLit      Literal
  | AnnLam      bndr (AnnExpr bndr annot)
  | AnnApp      (AnnExpr bndr annot) (AnnExpr bndr annot)
  | AnnCase     (AnnExpr bndr annot) bndr Type [AnnAlt bndr annot]
  | AnnLet      (AnnBind bndr annot) (AnnExpr bndr annot)
  | AnnCast     (AnnExpr bndr annot) (annot, Coercion)
                   -- Put an annotation on the (root of) the coercion
  | AnnTick     CoreTickish (AnnExpr bndr annot)
  | AnnType     Type
  | AnnCoercion Coercion

-- | A clone of the 'Alt' type but allowing annotation at every tree node
data AnnAlt bndr annot = AnnAlt AltCon [bndr] (AnnExpr bndr annot)

-- | A clone of the 'Bind' type but allowing annotation at every tree node
data AnnBind bndr annot
  = AnnNonRec bndr (AnnExpr bndr annot)
  | AnnRec    [(bndr, AnnExpr bndr annot)]

-- | Takes a nested application expression and returns the function
-- being applied and the arguments to which it is applied
collectAnnArgs :: AnnExpr b a -> (AnnExpr b a, [AnnExpr b a])
collectAnnArgs expr
  = go expr []
  where
    go (_, AnnApp f a) as = go f (a:as)
    go e               as = (e, as)

collectAnnArgsTicks :: (CoreTickish -> Bool) -> AnnExpr b a
                       -> (AnnExpr b a, [AnnExpr b a], [CoreTickish])
collectAnnArgsTicks tickishOk expr
  = go expr [] []
  where
    go (_, AnnApp f a)  as ts = go f (a:as) ts
    go (_, AnnTick t e) as ts | tickishOk t
                              = go e as (t:ts)
    go e                as ts = (e, as, reverse ts)

deAnnotate :: AnnExpr bndr annot -> Expr bndr
deAnnotate (_, e) = deAnnotate' e

deAnnotate' :: AnnExpr' bndr annot -> Expr bndr
deAnnotate' (AnnType t)           = Type t
deAnnotate' (AnnCoercion co)      = Coercion co
deAnnotate' (AnnVar  v)           = Var v
deAnnotate' (AnnLit  lit)         = Lit lit
deAnnotate' (AnnLam  binder body) = Lam binder (deAnnotate body)
deAnnotate' (AnnApp  fun arg)     = App (deAnnotate fun) (deAnnotate arg)
deAnnotate' (AnnCast e (_,co))    = Cast (deAnnotate e) co
deAnnotate' (AnnTick tick body)   = Tick tick (deAnnotate body)

deAnnotate' (AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
deAnnotate' (AnnCase scrut v t alts)
  = Case (deAnnotate scrut) v t (map deAnnAlt alts)

deAnnAlt :: AnnAlt bndr annot -> Alt bndr
deAnnAlt (AnnAlt con args rhs) = Alt con args (deAnnotate rhs)

deAnnBind  :: AnnBind b annot -> Bind b
deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

-- | As 'collectBinders' but for 'AnnExpr' rather than 'Expr'
collectAnnBndrs :: AnnExpr bndr annot -> ([bndr], AnnExpr bndr annot)
collectAnnBndrs e
  = collect [] e
  where
    collect bs (_, AnnLam b body) = collect (b:bs) body
    collect bs body               = (reverse bs, body)

-- | As 'collectNBinders' but for 'AnnExpr' rather than 'Expr'
collectNAnnBndrs :: Int -> AnnExpr bndr annot -> ([bndr], AnnExpr bndr annot)
collectNAnnBndrs orig_n e
  = collect orig_n [] e
  where
    collect 0 bs body               = (reverse bs, body)
    collect n bs (_, AnnLam b body) = collect (n-1) (b:bs) body
    collect _ _  _                  = pprPanic "collectNBinders" $ int orig_n

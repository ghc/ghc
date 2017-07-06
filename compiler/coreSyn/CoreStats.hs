{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-2015
-}

-- | Functions to computing the statistics reflective of the "size"
-- of a Core expression
module CoreStats (
        -- * Expression and bindings size
        coreBindsSize, exprSize,
        CoreStats(..), coreBindsStats, exprStats,
    ) where

import BasicTypes
import CoreSyn
import Outputable
import Coercion
import Var
import Type (Type, typeSize)
import Id (isJoinId)

import Data.List (foldl')

data CoreStats = CS { cs_tm :: !Int    -- Terms
                    , cs_ty :: !Int    -- Types
                    , cs_co :: !Int    -- Coercions
                    , cs_vb :: !Int    -- Local value bindings
                    , cs_jb :: !Int }  -- Local join bindings

type CST = CoreStats -> CoreStats

instance Outputable CoreStats where
 ppr (CS { cs_tm = i1, cs_ty = i2, cs_co = i3, cs_vb = i4, cs_jb = i5 })
   = braces (sep [text "terms:"     <+> intWithCommas i1 <> comma,
                  text "types:"     <+> intWithCommas i2 <> comma,
                  text "coercions:" <+> intWithCommas i3 <> comma,
                  text "joins:"     <+> intWithCommas i5 <> char '/' <>
                                        intWithCommas (i4 + i5) ])

plusCS :: CoreStats -> CoreStats -> CoreStats
plusCS (CS { cs_tm = p1, cs_ty = q1, cs_co = r1, cs_vb = v1, cs_jb = j1 })
       (CS { cs_tm = p2, cs_ty = q2, cs_co = r2, cs_vb = v2, cs_jb = j2 })
  = CS { cs_tm = p1+p2, cs_ty = q1+q2, cs_co = r1+r2, cs_vb = v1+v2
       , cs_jb = j1+j2 }

plusCST :: CST -> CST -> CST
plusCST f g acc = g $! f acc

zeroCST, oneTMT :: CST
zeroCST = plusCS zeroCS
oneTMT = plusCS oneTM

zeroCS, oneTM :: CoreStats
zeroCS = CS { cs_tm = 0, cs_ty = 0, cs_co = 0, cs_vb = 0, cs_jb = 0 }
oneTM  = zeroCS { cs_tm = 1 }

sumCST :: (a -> CST) -> [a] -> CST
sumCST f = foldr (\a r s -> r $! f a $! s) zeroCST

coreBindsStats :: [CoreBind] -> CoreStats
coreBindsStats binds = sumCST (bindStats TopLevel) binds zeroCS

bindStats :: TopLevelFlag -> CoreBind -> CST
bindStats top_lvl (NonRec v r) = bindingStats top_lvl v r
bindStats top_lvl (Rec prs)    = sumCST (\(v,r) -> bindingStats top_lvl v r) prs

bindingStats :: TopLevelFlag -> Var -> CoreExpr -> CST
bindingStats top_lvl v r = letBndrStats top_lvl v `plusCST` exprStatsT r

bndrStats :: Var -> CST
bndrStats v = oneTMT `plusCST` tyStats (varType v)

letBndrStats :: TopLevelFlag -> Var -> CST
letBndrStats top_lvl v
  | isTyVar v || isTopLevel top_lvl = bndrStats v
  | isJoinId v = plusCS (oneTM { cs_jb = 1 }) `plusCST` ty_stats
  | otherwise  = plusCS (oneTM { cs_vb = 1 }) `plusCST` ty_stats
  where
    ty_stats = tyStats (varType v)

exprStats :: CoreExpr -> CoreStats
exprStats e = exprStatsT e zeroCS

exprStatsT :: CoreExpr -> CST
exprStatsT (Var {})        = oneTMT
exprStatsT (Lit {})        = oneTMT
exprStatsT (Type t)        = tyStats t
exprStatsT (Coercion c)    = coStats c
exprStatsT (App f a)       = exprStatsT f `plusCST` exprStatsT a
exprStatsT (Lam b e)       = bndrStats b `plusCST` exprStatsT e
exprStatsT (Let b e)       = bindStats NotTopLevel b `plusCST` exprStatsT e
exprStatsT (Case e b _ as) = exprStatsT e `plusCST` bndrStats b
                                        `plusCST` sumCST altStats as
exprStatsT (Cast e co)     = coStats co `plusCST` exprStatsT e
exprStatsT (Tick _ e)      = exprStatsT e

altStats :: CoreAlt -> CST
altStats (_, bs, r) = altBndrStats bs `plusCST` exprStatsT r

altBndrStats :: [Var] -> CST
-- Charge one for the alternative, not for each binder
altBndrStats vs = oneTMT `plusCST` sumCST (tyStats . varType) vs

tyStats :: Type -> CST
tyStats ty = plusCS $ zeroCS { cs_ty = typeSize ty }

coStats :: Coercion -> CST
coStats co = plusCS $ zeroCS { cs_co = coercionSize co }

coreBindsSize :: [CoreBind] -> Int
-- We use coreBindStats for user printout
-- but this one is a quick and dirty basis for
-- the simplifier's tick limit
coreBindsSize bs = sum (map bindSize bs)

exprSize :: CoreExpr -> Int
-- ^ A measure of the size of the expressions, strictly greater than 0
-- Counts *leaves*, not internal nodes. Types and coercions are not counted.
exprSize (Var _)         = 1
exprSize (Lit _)         = 1
exprSize (App f a)       = exprSize f + exprSize a
exprSize (Lam b e)       = bndrSize b + exprSize e
exprSize (Let b e)       = bindSize b + exprSize e
exprSize (Case e b _ as) = exprSize e + bndrSize b + 1 + sum (map altSize as)
exprSize (Cast e _)      = 1 + exprSize e
exprSize (Tick n e)      = tickSize n + exprSize e
exprSize (Type _)        = 1
exprSize (Coercion _)    = 1

tickSize :: Tickish Id -> Int
tickSize (ProfNote _ _ _) = 1
tickSize _ = 1

bndrSize :: Var -> Int
bndrSize _ = 1

bndrsSize :: [Var] -> Int
bndrsSize = sum . map bndrSize

bindSize :: CoreBind -> Int
bindSize (NonRec b e) = bndrSize b + exprSize e
bindSize (Rec prs)    = sum (map pairSize prs)

pairSize :: (Var, CoreExpr) -> Int
pairSize (b,e) = bndrSize b + exprSize e

altSize :: CoreAlt -> Int
altSize (_,bs,e) = bndrsSize bs + exprSize e

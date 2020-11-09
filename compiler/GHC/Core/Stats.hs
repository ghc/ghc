{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-2015
-}

-- | Functions to computing the statistics reflective of the "size"
-- of a Core expression
module GHC.Core.Stats (
        -- * Expression and bindings size
        coreBindsSize, exprSize,
        CoreStats(..), coreBindsStats, exprStats,
    ) where

import GHC.Prelude

import GHC.Types.Basic
import GHC.Core
import GHC.Utils.Outputable
import GHC.Core.Coercion
import GHC.Types.Var
import GHC.Core.Type(Type, typeSize)
import GHC.Types.Id (isJoinId)

data CoreStats = CS { cs_tm :: !Int    -- Terms
                    , cs_ty :: !Int    -- Types
                    , cs_co :: !Int    -- Coercions
                    , cs_vb :: !Int    -- Local value bindings
                    , cs_jb :: !Int }  -- Local join bindings


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

zeroCS, oneTM :: CoreStats
zeroCS = CS { cs_tm = 0, cs_ty = 0, cs_co = 0, cs_vb = 0, cs_jb = 0 }
oneTM  = zeroCS { cs_tm = 1 }

sumCS :: (a -> CoreStats) -> [a] -> CoreStats
sumCS f = foldl' (\s a -> plusCS s (f a)) zeroCS

coreBindsStats :: [CoreBind] -> CoreStats
coreBindsStats = sumCS (bindStats TopLevel)

bindStats :: TopLevelFlag -> CoreBind -> CoreStats
bindStats top_lvl (NonRec v r) = bindingStats top_lvl v r
bindStats top_lvl (Rec prs)    = sumCS (\(v,r) -> bindingStats top_lvl v r) prs

bindingStats :: TopLevelFlag -> Var -> CoreExpr -> CoreStats
bindingStats top_lvl v r = letBndrStats top_lvl v `plusCS` exprStats r

bndrStats :: Var -> CoreStats
bndrStats v = oneTM `plusCS` tyStats (varType v)

letBndrStats :: TopLevelFlag -> Var -> CoreStats
letBndrStats top_lvl v
  | isTyVar v || isTopLevel top_lvl = bndrStats v
  | isJoinId v = oneTM { cs_jb = 1 } `plusCS` ty_stats
  | otherwise  = oneTM { cs_vb = 1 } `plusCS` ty_stats
  where
    ty_stats = tyStats (varType v)

exprStats :: CoreExpr -> CoreStats
exprStats (Var {})        = oneTM
exprStats (Lit {})        = oneTM
exprStats (Type t)        = tyStats t
exprStats (Coercion c)    = coStats c
exprStats (App f a)       = exprStats f `plusCS` exprStats a
exprStats (Lam b e)       = bndrStats b `plusCS` exprStats e
exprStats (Let b e)       = bindStats NotTopLevel b `plusCS` exprStats e
exprStats (Case e b _ as) = exprStats e `plusCS` bndrStats b
                                        `plusCS` sumCS altStats as
exprStats (Cast e co)     = coStats co `plusCS` exprStats e
exprStats (Tick _ e)      = exprStats e

altStats :: CoreAlt -> CoreStats
altStats (_, bs, r) = altBndrStats bs `plusCS` exprStats r

altBndrStats :: [Var] -> CoreStats
-- Charge one for the alternative, not for each binder
altBndrStats vs = oneTM `plusCS` sumCS (tyStats . varType) vs

tyStats :: Type -> CoreStats
tyStats ty = zeroCS { cs_ty = typeSize ty }

coStats :: Coercion -> CoreStats
coStats co = zeroCS { cs_co = coercionSize co }

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

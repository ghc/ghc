-- | Metaprogramming types
module GHC.Types.Meta
   ( MetaRequest(..)
   , MetaHook
   , MetaResult -- data constructors not exported to ensure correct response type
   , metaRequestE
   , metaRequestP
   , metaRequestT
   , metaRequestD
   , metaRequestAW
   )
where

import GHC.Prelude

import GHC.Serialized   ( Serialized )

import GHC.Hs
import GHC.Utils.Outputable
import GHC.Utils.Panic


-- | The supported metaprogramming result types
data MetaRequest
  = MetaE  (LHsExpr GhcPs   -> MetaResult)
  | MetaP  (LPat GhcPs      -> MetaResult)
  | MetaT  (LHsType GhcPs   -> MetaResult)
  | MetaD  ([LHsDecl GhcPs] -> MetaResult)
  | MetaAW (Serialized     -> MetaResult)

-- | data constructors not exported to ensure correct result type
data MetaResult
  = MetaResE  (LHsExpr GhcPs)
  | MetaResP  (LPat GhcPs)
  | MetaResT  (LHsType GhcPs)
  | MetaResD  [LHsDecl GhcPs]
  | MetaResAW Serialized

instance Outputable MetaResult where
    ppr (MetaResE e)   = text "MetaResE"  <> braces (ppr e)
    ppr (MetaResP p)   = text "MetaResP"  <> braces (ppr p)
    ppr (MetaResT t)   = text "MetaResT"  <> braces (ppr t)
    ppr (MetaResD d)   = text "MetaResD"  <> braces (ppr d)
    ppr (MetaResAW aw) = text "MetaResAW" <> braces (ppr aw)

-- These unMetaResE ext panics will triger if the MetaHook doesn't
-- take an expression to an expression, pattern to pattern etc.
--
-- ToDo: surely this could be expressed in the type system?
unMetaResE :: MetaResult -> LHsExpr GhcPs
unMetaResE (MetaResE e) = e
unMetaResE mr           = pprPanic "unMetaResE" (ppr mr)

unMetaResP :: MetaResult -> LPat GhcPs
unMetaResP (MetaResP p) = p
unMetaResP mr           = pprPanic "unMetaResP" (ppr mr)

unMetaResT :: MetaResult -> LHsType GhcPs
unMetaResT (MetaResT t) = t
unMetaResT mr           = pprPanic "unMetaResT" (ppr mr)

unMetaResD :: MetaResult -> [LHsDecl GhcPs]
unMetaResD (MetaResD d) = d
unMetaResD mr           = pprPanic "unMetaResD" (ppr mr)

unMetaResAW :: MetaResult -> Serialized
unMetaResAW (MetaResAW aw) = aw
unMetaResAW mr             = pprPanic "unMetaResAW" (ppr mr)

type MetaHook f = MetaRequest -> LHsExpr GhcTc -> f MetaResult

metaRequestE :: Functor f => MetaHook f -> LHsExpr GhcTc -> f (LHsExpr GhcPs)
metaRequestE h = fmap unMetaResE . h (MetaE MetaResE)

metaRequestP :: Functor f => MetaHook f -> LHsExpr GhcTc -> f (LPat GhcPs)
metaRequestP h = fmap unMetaResP . h (MetaP MetaResP)

metaRequestT :: Functor f => MetaHook f -> LHsExpr GhcTc -> f (LHsType GhcPs)
metaRequestT h = fmap unMetaResT . h (MetaT MetaResT)

metaRequestD :: Functor f => MetaHook f -> LHsExpr GhcTc -> f [LHsDecl GhcPs]
metaRequestD h = fmap unMetaResD . h (MetaD MetaResD)

metaRequestAW :: Functor f => MetaHook f -> LHsExpr GhcTc -> f Serialized
metaRequestAW h = fmap unMetaResAW . h (MetaAW MetaResAW)


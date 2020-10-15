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


-- | The supported metaprogramming result types
data MetaRequest
  = MetaE  (LHsExpr GhcPs   -> MetaResult)
  | MetaP  (LPat GhcPs      -> MetaResult)
  | MetaT  (LHsType GhcPs   -> MetaResult)
  | MetaD  ([LHsDecl GhcPs] -> MetaResult)
  | MetaAW (Serialized     -> MetaResult)

-- | data constructors not exported to ensure correct result type
data MetaResult
  = MetaResE  { unMetaResE  :: LHsExpr GhcPs   }
  | MetaResP  { unMetaResP  :: LPat GhcPs      }
  | MetaResT  { unMetaResT  :: LHsType GhcPs   }
  | MetaResD  { unMetaResD  :: [LHsDecl GhcPs] }
  | MetaResAW { unMetaResAW :: Serialized      }

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


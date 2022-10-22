
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module DCo_HsType ( hsWcScopedTvs ) where

import Prelude (undefined)

data GhcPass (c :: Pass)
data Pass = Renamed | Typechecked
type GhcRn   = GhcPass 'Renamed

data HsTyVarBndr pass
type LHsTyVarBndr pass = XRec pass (HsTyVarBndr pass)

type LHsSigType   pass = XRec pass (HsSigType pass)
type LHsSigWcType pass = HsWildCardBndrs pass (LHsSigType pass)

type HsOuterSigTyVarBndrs = HsOuterTyVarBndrs
data HsSigType pass
  = HsSig { sig_bndrs :: HsOuterSigTyVarBndrs pass }

data HsWildCardBndrs pass thing
  = HsWC { hswc_body :: thing }

data HsOuterTyVarBndrs pass

type family NoGhcTc p
type instance NoGhcTc (GhcPass pass) = GhcPass (NoGhcTcPass pass)

type family NoGhcTcPass (p :: Pass) :: Pass where
  NoGhcTcPass 'Typechecked = 'Renamed
  NoGhcTcPass other        = other

type family   XRec p           a
type instance XRec (GhcPass p) a = a

hsOuterExplicitBndrs :: HsOuterTyVarBndrs (GhcPass p)
                     -> LHsTyVarBndr (NoGhcTc (GhcPass p))
hsOuterExplicitBndrs = undefined

hsWcScopedTvs :: LHsSigWcType GhcRn -> LHsTyVarBndr GhcRn
hsWcScopedTvs sig_wc_ty
  | HsWC { hswc_body = sig_ty } <- sig_wc_ty
  , HsSig { sig_bndrs = outer_bndrs } <- sig_ty
  = hsOuterExplicitBndrs outer_bndrs

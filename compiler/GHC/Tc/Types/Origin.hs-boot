module GHC.Tc.Types.Origin where

import GHC.Prelude.Basic ( Int, Maybe )
import GHC.Stack ( HasCallStack )
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type )

data SkolemInfoAnon
data SkolemInfo
data FixedRuntimeRepContext
data FixedRuntimeRepOrigin
  = FixedRuntimeRepOrigin
    { frr_type    :: Type
    , frr_context :: FixedRuntimeRepContext
    }

mkFRRUnboxedTuple :: Int -> FixedRuntimeRepContext
mkFRRUnboxedSum :: Maybe Int -> FixedRuntimeRepContext

unkSkol :: HasCallStack => SkolemInfo

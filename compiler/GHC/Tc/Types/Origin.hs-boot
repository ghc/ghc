module GHC.Tc.Types.Origin where

import GHC.Prelude.Basic ( Int, Maybe )
import GHC.Utils.Misc ( HasDebugCallStack )
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

unkSkol :: HasDebugCallStack => SkolemInfo

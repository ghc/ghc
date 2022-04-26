module GHC.Tc.Types.Origin where

import GHC.Stack ( HasCallStack )

data SkolemInfoAnon
data SkolemInfo
data FixedRuntimeRepContext
data FixedRuntimeRepOrigin

unkSkol :: HasCallStack => SkolemInfo

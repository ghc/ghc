module GHC.Tc.Types.Origin where

import GHC.Stack ( HasCallStack )

data SkolemInfoAnon
data SkolemInfo

unkSkol :: HasCallStack => SkolemInfo

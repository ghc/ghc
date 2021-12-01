module GHC.Tc.Types.Origin where

import GHC.Stack ( HasCallStack )

data SkolemInfo

unkSkol :: HasCallStack => SkolemInfo

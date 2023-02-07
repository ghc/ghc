module GHC.Tc.Types.Origin where

import GHC.Stack ( HasCallStack )

data SkolemInfoAnon
data SkolemInfo
data FixedRuntimeRepContext
data FixedRuntimeRepOrigin

data CtOrigin
data ClsInstOrQC = IsClsInst
                 | IsQC CtOrigin

unkSkol :: HasCallStack => SkolemInfo

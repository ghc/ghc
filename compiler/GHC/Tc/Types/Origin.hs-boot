module GHC.Tc.Types.Origin where

import GHC.Utils.Misc ( HasDebugCallStack )

data SkolemInfoAnon
data SkolemInfo
data FixedRuntimeRepContext
data FixedRuntimeRepOrigin

data CtOrigin
data ClsInstOrQC = IsClsInst
                 | IsQC CtOrigin

unkSkol :: HasDebugCallStack => SkolemInfo

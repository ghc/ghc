{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module IfaceSharingName where

import Lib
import GHC.Data.FastString
import GHC.Builtin.Uniques
import GHC.Builtin.Names
import GHC.Types.Name
import GHC.Types.SrcLoc

[f1,f2,f3,f4,f5] = map mkVarOcc ["a", "b","c","d","e"]

[u1,u2,u3,u4,u5] = map mkPreludeMiscIdUnique [10000..10004]

names = [ mkExternalName u1 pRELUDE f1 noSrcSpan
        , mkExternalName u2 pRELUDE f2 noSrcSpan
        , mkExternalName u3 pRELUDE f3 noSrcSpan
        , mkExternalName u4 pRELUDE f4 noSrcSpan
        , mkExternalName u5 pRELUDE f5 noSrcSpan ]

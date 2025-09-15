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

m = gHC_PRIM

names = [ mkExternalName u1 m f1 noSrcSpan
        , mkExternalName u2 m f2 noSrcSpan
        , mkExternalName u3 m f3 noSrcSpan
        , mkExternalName u4 m f4 noSrcSpan
        , mkExternalName u5 m f5 noSrcSpan ]

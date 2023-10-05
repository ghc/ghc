{-# LANGUAGE Haskell2010 #-}
module Swish.GraphMatch where

import qualified Data.Map as M
import Data.Word (Word32)

class Label lb

type LabelIndex = (Word32, Word32)

data (Label lb, Eq lv, Show lv) => GenLabelMap lb lv =
    MkLabelMap Word32 (M.Map lb lv)

type LabelMap lb = GenLabelMap lb LabelIndex

emptyMap :: Label lb => LabelMap lb
emptyMap = MkLabelMap 1 M.empty

-- MkLabelMap :: forall lb lv. (Label lb, Eq lv, Show lv)
--            => Word32 -> M.Map lb lv -> GenLabelMap lb lv
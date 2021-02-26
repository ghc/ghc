module GHC.Core.ConLike where

import {-# SOURCE #-} GHC.Core.DataCon

data ConLike

mkRealDataCon :: DataCon -> ConLike

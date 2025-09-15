{-# LANGUAGE TypeFamilies #-}
module T19773b where
import T19773a

-- Should warn for orphan instance
data instance DF [a] = DF_B a

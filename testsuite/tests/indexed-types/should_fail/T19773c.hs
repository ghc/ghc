{-# LANGUAGE TypeFamilies #-}
module T19773c where
import T19773a

-- Should warn for orphan instance
data instance DF [a] = DF_C a

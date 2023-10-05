{-# LANGUAGE NoFieldSelectors #-}
module NFSExport (T(foo), foo) where -- only T(foo) is supported
data T = MkT { foo :: T }

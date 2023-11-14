{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module T24083a where

type TyCon :: (k1 -> k2) -> unmatchable_fun
data family TyCon :: (k1 -> k2) -> unmatchable_fun

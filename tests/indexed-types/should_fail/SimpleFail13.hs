{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

type family C a :: *

data family D a :: *
-- must fail: lhs contains a type family application
data instance D [C a] = DC

type family E a :: *
-- must fail: lhs contains a type family application
type instance E [C a] = Int

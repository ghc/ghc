
{-# LANGUAGE TypeFamilies #-}
module Tc245_A where
class Foo a where
    data Bar a :: * -> *

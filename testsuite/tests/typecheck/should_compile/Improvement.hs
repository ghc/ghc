{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Foo where

type family F a
type instance F Int = Bool

class C a b where

instance (b~Int) => C Bool b

blug :: C (F a) a => a -> F a
blug = error "Urk"

foo :: Bool
foo = blug undefined
-- [W] C (F a0) a0, F a0 ~ Bool


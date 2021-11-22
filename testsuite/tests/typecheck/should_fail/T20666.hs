{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

type family T t
type family S s

class Show (T c) => C1 c
class Show (T (S d)) => D d
instance (D d, c ~ S d) => C1 c
 -- this one fails in GHC 9.2

class Show (T c) => C2 c
instance (D d, c ~ S d, c' ~ c) => C2 c'
 -- This one succeeded because it went via lookupInInerts.
 -- It should fail, just like the one above.

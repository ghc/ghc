{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module T18145 where

type family A :: k
type instance forall. A = Nothing :: Maybe a -- 'a' should be out of scope

class Foo x where
  type B x :: Maybe a
  type forall x. B x = Nothing :: Maybe a -- 'a' should be out of scope

instance Foo [x] where
  type forall. B [x] = Nothing :: Maybe a -- 'a' should be out of scope

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module T7347 where

data K = forall a. T a  -- Existential: promotion gives 'T :: forall k. k -> K                          

data G :: K -> * where
  D :: G (T [])         -- Uses existential

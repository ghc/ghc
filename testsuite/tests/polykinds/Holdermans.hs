{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Holdermans where

data K = forall a. T a  -- promotion gives 'T :: * -> K                          

data G :: K -> * where
  D :: G (T [])         -- kind error!

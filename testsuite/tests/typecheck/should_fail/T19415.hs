{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Test where

import GHC.TypeLits

data Pet (a :: k) = Pet
  { name :: String
  } deriving Show

class SetField (name :: Symbol) s t b | name t -> s b
                                      , name s -> t b where
  setField :: b -> s -> t

instance
  ( SetField "name" (Pet a) (Pet b) String
  ) => SetField "name" (Pet (a :: k)) (Pet (b :: k')) String where
  setField v d = d { name = v }

loop = setField @"name" 'c' (Pet "hi")

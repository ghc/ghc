{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T18021 where

class C a where
  data D a

instance forall a. C Int where
  data instance D Int = MkD1 a

class X a b

instance forall a. C Bool where
  data instance D Bool = MkD2
    deriving (X a)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module T11145 where

data family Fuggle x y

[d| data instance Fuggle Int (Maybe (a,b)) where
      MkFuggle :: Fuggle Int (Maybe Bool) |]

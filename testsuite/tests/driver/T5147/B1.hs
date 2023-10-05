{-# LANGUAGE TypeFamilies #-}
module B where

class Entity v where
   data Fields v

instance Show (Fields v) where show = undefined


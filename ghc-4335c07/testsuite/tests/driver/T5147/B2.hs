{-# LANGUAGE TypeFamilies #-}
module B where

class Entity v where
   data Fields v

-- Remove instance
-- instance Show (Fields v) where show = undefined

